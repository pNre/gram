open Async
open Core
open Cli
open Tdlib
open Tdlib.Models

let api_id = Sys.getenv_exn "TG_API_ID"
let api_hash = Sys.getenv_exn "TG_API_HASH"
let db_key = Sys.getenv_exn "TG_DB_KEY"

let request_chats state client chat_ids =
  let open Option.Let_syntax in
  let open Models in
  let request =
    let%bind chat_id = List.last chat_ids in
    let%map chat = State.chat state chat_id in
    let offset_chat_id = Chat.id chat in
    let offset_order = Chat.order chat in
    Request.Get_chats (Chat.Request.Get_chats.create ~offset_chat_id ~offset_order ())
  in
  Option.iter request ~f:(Client.send client)
;;

let handle_message state ?(prefix = "") message =
  let content = message |> Message.content |> Display.Message.display_content in
  let sender_user_id = Message.sender_user_id message in
  let sender_user_id_hash = User.Id.hash sender_user_id in
  let sender_style = [ Style.Foreground (Color.gen_color sender_user_id_hash) ] in
  let chat = State.chat state (Message.chat_id message) in
  let user = State.user state sender_user_id in
  let title =
    match chat, user with
    | Some chat, Some user -> Display.Update.prefix chat user
    | Some chat, None -> Chat.title chat
    | None, Some user -> User.full_name user
    | None, None -> User.Id.to_string sender_user_id
  in
  printf [] "%s%s%s\n" prefix (Style.with_format sender_style title ^ ": ") content
;;

let handle_user_status state update =
  let open User.Request in
  let user_id = Update_status.user_id update in
  let status = Update_status.status update in
  let new_state =
    let open Option.Let_syntax in
    let%bind user = State.user state user_id
    and description = Display.User.Status.to_description_string status in
    let%map result = State.update_user_status state user status in
    printf Style.secondary "%s: %s\n" (User.full_name user) description;
    result
  in
  Option.value new_state ~default:state
;;

let handle_chat_action state update =
  let open Chat.Request in
  let action = Update_action.action update in
  let new_state =
    let open Option.Let_syntax in
    let%bind chat = update |> Update_action.chat_id |> State.chat state
    and user = update |> Update_action.user_id |> State.user state in
    let prefix = Display.Update.prefix chat user in
    let result, state = State.update_chat_action state action user chat in
    let%map description = Display.Chat.Action.to_description_string result in
    printf Style.secondary "%s: %s\n" prefix description;
    state
  in
  Option.value new_state ~default:state
;;

let dispatch dbpath state client = function
  | Request.Update_authorization_state Wait_tdlib_parameters ->
    let database_directory =
      match dbpath with
      | Some path -> path
      | None -> Sys.getenv_exn "HOME" ^ "/.config/db"
    in
    let parameters =
      Tdlib.Parameters.create
        ~database_directory
        ~api_id
        ~api_hash
        ~use_message_database:true
        ~enable_storage_optimizer:true
        ~use_secret_chats:true
        ~system_language_code:"en"
        ~device_model:"Caml"
        ~system_version:"1.0"
        ~application_version:"0.0.1"
    in
    Client.send client (Set_tdlib_parameters parameters);
    state
  | Update_authorization_state Wait_encryption_key ->
    Client.send client (Check_database_encryption_key db_key);
    state
  | Update_authorization_state Wait_phone_number ->
    let phone_number = Readline.readline "Phone number: " in
    Client.send client (Set_authentication_phone_number phone_number);
    state
  | Update_authorization_state Wait_code ->
    let auth_code = Readline.readline "Authentication code: " in
    Client.send client (Check_authentication_code auth_code);
    state
  | Update_authorization_state Ready ->
    Ivar.fill state.State.is_ready ();
    Client.send client Get_contacts;
    Client.send client (Get_chats (Chat.Request.Get_chats.create ()));
    state
  | Chats chat_ids ->
    List.iter chat_ids ~f:(fun id -> Client.send client (Get_chat id));
    request_chats state client chat_ids;
    let current_chat_ids = state.chat_ids in
    State.set_chat_ids state (current_chat_ids @ chat_ids)
  | Message message when State.is_message_id_updated state (Message.id message) ->
    handle_message ~prefix:"(Updated) " state message;
    State.remove_updated_message_id state (Message.id message)
  | Update_new_message message ->
    handle_message state message;
    State.append_unread_message_id state (Message.chat_id message) (Message.id message)
  | Update_message_content request ->
    let open Message.Request in
    let chat_id = Update_content.chat_id request in
    let message_id = Update_content.message_id request in
    let request = Get.create ~chat_id ~message_id in
    Client.send client (Get_message request);
    State.append_updated_message_id state message_id
  | Chat chat | Update_new_chat chat -> State.set_chat state chat
  | Update_user user -> State.set_user state user
  | Update_user_status update -> handle_user_status state update
  | Update_user_chat_action update -> handle_chat_action state update
  | Update_file file ->
    let local_file = File.local file in
    printf
      Style.secondary
      !"File: %{Style#success}\n"
      (Display.File.Local.to_string local_file);
    state
  | _ -> state
;;

let debug_log = function
  | `Sent -> printf Style.tertiary "→ %s\n"
  | `Received -> printf Style.tertiary "← %s\n"
;;

let cli client state () =
  let%bind _ = State.when_ready (Mvar.peek_exn state) in
  Cli.parse (fun cmd ->
      Mvar.update_exn state ~f:(fun state -> Cmds.exec client state cmd))
;;

let receive dbpath client state () =
  let%map msg = Client.receive client in
  Mvar.update_exn state ~f:(fun state ->
      match msg with
      | Some (Ok req) ->
        let state = dispatch dbpath state client req.typ in
        State.run_mutations state req
      | Some (Error err) ->
        printf [] !"Error parsing message: %{Style#error}\n" (Exn.to_string err);
        state
      | None -> state)
;;

let start dbpath debug =
  ignore (Client.execute (Models.Request.Set_log_verbosity_level Error));
  let debug = if debug then debug_log else fun _ _ -> () in
  let client = Client.init ~debug () in
  let state = State.create () in
  Cli.configure state;
  Deferred.forever () (cli client state);
  Deferred.forever () (receive dbpath client state);
  Shutdown.at_shutdown (fun _ -> return (Client.deinit client));
  Deferred.never ()
;;

let () =
  let open Async.Command in
  let open Command.Let_syntax in
  async
    ~summary:""
    [%map_open
      let dbpath =
        flag
          "database"
          (optional string)
          ~doc:"path Database folder path, defaults to ~/.config/gram"
      and debug = flag "debug" (optional bool) ~doc:"true|false Debug mode enabled" in
      fun () -> start dbpath (Option.value ~default:false debug)]
  |> Command.run
;;
