open Async
open Core
open Tdlib

let api_id = Sys.getenv_exn "TG_API_ID"
let api_hash = Sys.getenv_exn "TG_API_HASH"

let dispatch ~db_path ~debug_mode state client response =
  let open Models.Request in
  if debug_mode
  then
    Cli.print
      ~style:[ `Dim ]
      (sprintf "%s\n" (Yojson.Safe.to_string (yojson_of_t response)));
  match response with
  | Update_authorization_state Wait_tdlib_parameters ->
    let db_path =
      match db_path with
      | Some path -> path
      | None -> Sys.getenv_exn "HOME" ^ "/.config/db"
    in
    let parameters =
      Models.Tdlib.Parameters.create
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
    Client.send client (Check_database_encryption_key (Sys.getenv_exn "TG_DB_KEY"));
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
    Client.send client (Get_chats (Models.Chat.Request.Get_chats.create ()));
    state
  | Chats chat_ids ->
    List.iter chat_ids ~f:(fun id -> Client.send client (Get_chat id));
    (match chat_ids with
    | [] -> ()
    | chats ->
      let chat = chats |> List.last_exn |> State.chat state in
      Option.iter chat ~f:(fun chat ->
          let offset_chat_id = Models.Chat.id chat in
          let offset_order = Models.Chat.order chat in
          let request =
            Models.Chat.Request.Get_chats.create ~offset_chat_id ~offset_order ()
          in
          Client.send client (Get_chats request)));
    State.set_chat_ids state chat_ids
  | Update_new_message message ->
    let key = Some (Int32.hash (Models.Message.sender_user_id message)) in
    let description =
      Display.Message.to_description_string message (State.chat state) (State.user state)
    in
    Option.iter description ~f:(fun description ->
        Cli.print ~style:[ `Bright ] ~key (sprintf "%s\n" description));
    State.append_unread_message state message
  | Chat chat | Update_new_chat chat -> State.set_chat state chat
  | Update_user user -> State.set_user state user
  | Update_user_status update ->
    let open Models.User.Request.Update_status in
    let user_id = user_id update in
    let status = status update in
    let user = State.user state user_id in
    let status = Display.User.Status.to_description_string status in
    Option.iter (Option.both user status) ~f:(fun (user, status) ->
        Cli.print
          ~style:[ `Dim; `Yellow ]
          (sprintf "%s: %s\n" (Models.User.full_name user) status));
    state
  | Update_user_chat_action update ->
    let open Models.User.Request.Update_chat_action in
    let action = action update in
    let chat_id = chat_id update in
    let user_id = user_id update in
    let chat = State.chat state chat_id in
    let user = State.user state user_id in
    let prefix = Display.Update.prefix chat user in
    let action = Display.Chat.Action.to_description_string action in
    Cli.print ~style:[ `Dim; `Yellow ] (sprintf "%s: %s\n" prefix action);
    state
  | Update_file file ->
    Cli.print
      ~style:[ `Green ]
      (sprintf
         "File: %s\n"
         (Display.File.Local.to_description_string (Models.File.local file)));
    state
  | _ -> state
;;

let cli client state () =
  State.when_ready (Mvar.peek_exn state) >>= Cli.parse client state Cmds.exec
;;

let receive_and_dispatch ~db_path ~debug_mode client state () =
  Client.receive client
  >>= (fun msg ->
        let state = Mvar.peek_exn state in
        match msg with
        | None -> return state
        | Some (Ok response) ->
          return (dispatch ~db_path ~debug_mode state client response)
        | Some (Error err) ->
          Cli.print (sprintf "Error parsing message: %s\n" (Exn.to_string err));
          return state)
  >>| fun new_state -> Mvar.set state new_state
;;

let start ~db_path ~debug_mode =
  ignore (Client.execute (Models.Request.Set_log_verbosity_level Error));
  let client = Client.init () in
  let state = Mvar.create () in
  Mvar.set state State.empty;
  Cli.configure state;
  Deferred.forever () (cli client state);
  Deferred.forever () (receive_and_dispatch ~db_path ~debug_mode client state);
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
          and debug_mode =
            flag "debug" (optional bool) ~doc:"true|false Debug mode enabled"
          in
          fun () -> start ~db_path ~debug_mode:(Option.value ~default:false debug_mode)])
  in
  Command.run command
;;
