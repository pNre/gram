open Async
open Core
open Tdlib
open Tdlib.Models

let api_id = Sys.getenv_exn "TG_API_ID"
let api_hash = Sys.getenv_exn "TG_API_HASH"
let db_key = Sys.getenv_exn "TG_DB_KEY"

let request_chats state client chat_ids =
  let open Models in
  if not (List.is_empty chat_ids)
  then
    chat_ids
    |> List.last_exn
    |> State.chat state
    |> Option.iter ~f:(fun chat ->
           let offset_chat_id = Chat.id chat in
           let offset_order = Chat.order chat in
           let request = Chat.Request.Get_chats.create ~offset_chat_id ~offset_order () in
           Client.send client (Get_chats request))
;;

let print_message state ?(prefix = "") message =
  let content = Message.content message in
  let sender_user_id = Message.sender_user_id message in
  let key = Some (Int32.hash sender_user_id) in
  let chat = State.chat state (Message.chat_id message) in
  let user = State.user state sender_user_id in
  let description = Display.Message.to_description_string chat user content in
  Option.iter description ~f:(fun description ->
      Cli.print ~style:[ `Bright ] ~key (sprintf "%s%s\n" prefix description))
;;

let print_user_status state update =
  let open User.Request in
  let user_id = Update_status.user_id update in
  let status = Update_status.status update in
  let user = State.user state user_id in
  let description = Display.User.Status.to_description_string status in
  let result =
    Option.map user ~f:(fun user -> State.update_user_status state user status)
  in
  match result with
  | Some (`Ok state) ->
    Option.both user description
    |> Option.iter ~f:(fun (user, description) ->
           Cli.print
             ~style:[ `Dim; `Yellow ]
             (sprintf "%s: %s\n" (User.full_name user) description));
    state
  | _ -> state
;;

let print_chat_action state update =
  let open User.Request in
  let action = Update_chat_action.action update in
  let chat_id = Update_chat_action.chat_id update in
  let user_id = Update_chat_action.user_id update in
  let chat = State.chat state chat_id in
  let user = State.user state user_id in
  let prefix = Display.Update.prefix chat user in
  let result = Option.map2 user chat ~f:(State.update_chat_action state action) in
  let state = Option.value_map result ~default:state ~f:snd in
  result
  |> Option.map ~f:fst
  |> Option.bind ~f:Display.Chat.Action.to_description_string
  |> Option.iter ~f:(fun op ->
         Cli.print ~style:[ `Dim; `Yellow ] (sprintf "%s: %s\n" prefix op));
  state
;;

let dispatch ~db_path state client = function
  | Request.Update_authorization_state Wait_tdlib_parameters ->
    let db_path =
      match db_path with
      | Some path -> path
      | None -> Sys.getenv_exn "HOME" ^ "/.config/db"
    in
    let parameters =
      Tdlib.Parameters.create
        ~database_directory:db_path
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
    print_message ~prefix:"(Updated) " state message;
    State.remove_updated_message_id state (Message.id message)
  | Update_new_message message ->
    print_message state message;
    State.append_unread_message state message
  | Update_message_content request ->
    let open Message.Request in
    let chat_id = Update_content.chat_id request in
    let message_id = Update_content.message_id request in
    let request = Get.create ~chat_id ~message_id in
    Client.send client (Get_message request);
    State.append_updated_message_id state message_id
  | Chat chat | Update_new_chat chat -> State.set_chat state chat
  | Update_user user -> State.set_user state user
  | Update_user_status update -> print_user_status state update
  | Update_user_chat_action update -> print_chat_action state update
  | Update_file file ->
    let description = Display.File.Local.to_description_string (File.local file) in
    Cli.print ~style:[ `Green ] (sprintf "File: %s\n" description);
    state
  | _ -> state
;;

let log_if_debug ~debug request =
  if debug
  then (
    let s = Yojson.Safe.to_string (Models.Request.yojson_of_t request) in
    Cli.print ~style:[ `Dim ] (s ^ "\n"))
;;

let cli client state () =
  State.when_ready (Mvar.peek_exn state) >>= Cli.parse client state Cmds.exec
;;

let receive_and_dispatch ~db_path ~debug client state () =
  Client.receive client
  >>= (fun msg ->
        let state = Mvar.peek_exn state in
        let result =
          match msg with
          | None -> state
          | Some (Ok request) ->
            log_if_debug ~debug request;
            dispatch ~db_path state client request
          | Some (Error err) ->
            Cli.print (sprintf "Error parsing message: %s\n" (Exn.to_string err));
            state
        in
        return result)
  >>| Mvar.set state
;;

let start ~db_path ~debug =
  ignore (Client.execute (Models.Request.Set_log_verbosity_level Error));
  let client = Client.init () in
  let state = Mvar.create () in
  Mvar.set state State.empty;
  Cli.configure state;
  Deferred.forever () (cli client state);
  Deferred.forever () (receive_and_dispatch ~db_path ~debug client state);
  Shutdown.at_shutdown (fun _ -> return (Client.deinit client));
  Deferred.never ()
;;

let () =
  let command =
    Async.Command.async
      ~summary:""
      Command.Let_syntax.(
        [%map_open
          let db_path =
            flag
              "database"
              (optional string)
              ~doc:"path Database folder path, defaults to ~/.config/gram"
          and debug = flag "debug" (optional bool) ~doc:"true|false Debug mode enabled" in
          fun () -> start ~db_path ~debug:(Option.value ~default:false debug)])
  in
  Command.run command
;;
