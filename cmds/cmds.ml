open Core
open Core.Poly
open Cli
open Tdlib
open Tdlib.Models

module Parser = struct
  open Angstrom
  open Char

  let escape = '\\'
  let quote = '"'
  let is_identifier c = (not (is_whitespace c)) && not (c = quote)
  let identifier = take_while1 is_identifier <?> "identifier"
  let whitespaces = take_while is_whitespace <?> "whitespaces"
  let escaped_space = char escape *> satisfy is_whitespace
  let escaped_quote = char escape *> char quote

  let string_literal =
    let quoted =
      char quote *> many (escaped_quote <|> not_char quote)
      <* char quote
      <?> "quoted string"
    in
    let escaped =
      many1 (escaped_space <|> escaped_quote <|> satisfy is_identifier)
      <?> "escaped string"
    in
    quoted <|> escaped >>| String.of_char_list
  ;;

  let parse =
    parse_string
      (many_till
         (whitespaces *> (string_literal <|> identifier) <* whitespaces)
         end_of_input)
  ;;
end

module Command = struct
  module Arg = struct
    type t =
      { name : string
      ; summary : string option
      }
    [@@deriving fields]

    let description ~sep arg =
      [ Some (sprintf [ Bold ] "%s" (name arg)); summary arg ]
      |> List.filter_opt
      |> String.concat ~sep
    ;;
  end

  type t =
    { name : string
    ; shape : Arg.t list
    ; summary : string
    ; handler : Client.t -> State.t -> string list -> State.t
    }
  [@@deriving fields]

  let create = Fields.create
  let arg ~name ~summary = Arg.Fields.create ~name ~summary:(Some summary)
  let anon = Arg.Fields.create ~summary:None

  let description command =
    sprintf
      []
      !"\t%{Style#prominent}\n\t    %s\n\t    - %s\n\n"
      (name command)
      (summary command)
      (shape command
      |> List.map ~f:(Arg.description ~sep:":\n\t      ")
      |> String.concat ~sep:"\n\t    - ")
  ;;
end

(*Sends a message*)
let message client state args =
  let send_message recipient message =
    let chat = State.find_chat state recipient in
    Option.iter chat ~f:(fun chat ->
        let open Models.Message.Content in
        let chat_id = Models.Chat.id chat in
        let input_message_content = Input.create_text message in
        let req = Models.Message.Request.Send.create chat_id input_message_content in
        Client.send client (Send_message req));
    state
  in
  match args with
  | recipient :: args -> send_message recipient (String.concat ~sep:" " args)
  | _ -> state
;;

(*Sends a photo*)
let photo client state args =
  let send_photo recipient caption path =
    let chat = State.find_chat state recipient in
    Option.iter chat ~f:(fun chat ->
        let open Models.Message.Content in
        let chat_id = Models.Chat.id chat in
        let input_message_content = Input.create_photo caption path in
        let req = Models.Message.Request.Send.create chat_id input_message_content in
        Client.send client (Send_message req));
    state
  in
  match args with
  | recipient :: caption :: path :: _ -> send_photo recipient caption path
  | _ -> state
;;

(* Marks a chat as read *)
let read client state args =
  args
  |> String.concat ~sep:" "
  |> State.find_chat state
  |> function
  | Some chat ->
    let open Models in
    let chat_id = Chat.id chat in
    let message_ids = State.unread_message_ids state chat_id in
    let req = Message.Request.View.create ~chat_id ~message_ids ~force_read:true in
    let request_uuid = Client.send' client (View_messages req) in
    State.add_mutation state (fun req state ->
        match req with
        | { uuid = Some uuid; typ = Ok } when uuid = request_uuid ->
          printf Style.tertiary !"%{Chat.title} marked as read\n" chat;
          `Apply (State.remove_unread_message_ids state chat_id)
        | _ -> `Skip)
  | None -> state
;;

(* Downloads a file *)
let file_download client state args =
  let req =
    let open Option.Let_syntax in
    let%map id = List.hd args in
    let file_id = Int32.of_string id in
    Request.Download_file
      (File.Request.Download.create
         ~file_id
         ~priority:1l
         ~offset:0l
         ~limit:0l
         ~synchronous:true)
  in
  Option.iter req ~f:(Client.send client);
  state
;;

(* Stops downloading a file *)
let file_download_cancel client state args =
  args
  |> List.hd
  |> Option.map ~f:Int32.of_string
  |> Option.iter ~f:(fun file_id ->
         Client.send client (Models.Request.Cancel_download_file file_id));
  state
;;

(* Commands *)
let commands =
  let open Command in
  [ create
      ~name:"m"
      ~shape:
        [ arg ~name:"recipient" ~summary:"Name of the recipient or chat"
        ; anon ~name:"message"
        ]
      ~summary:"Send a message"
      ~handler:message
  ; create
      ~name:"p"
      ~shape:
        [ arg ~name:"recipient" ~summary:"Name of the recipient or chat"
        ; anon ~name:"caption"
        ; arg ~name:"path" ~summary:"Path of the photo on the local filesystem"
        ]
      ~summary:"Send a photo"
      ~handler:photo
  ; create
      ~name:"r"
      ~shape:[ arg ~name:"chat" ~summary:"Name of the chat to mark as read" ]
      ~summary:"Mark a chat as read"
      ~handler:read
  ; create
      ~name:"fd"
      ~shape:[ arg ~name:"file" ~summary:"ID of a file download" ]
      ~summary:"Download a file"
      ~handler:file_download
  ; create
      ~name:"fdc"
      ~shape:[ arg ~name:"file" ~summary:"ID of the file to stop downloading" ]
      ~summary:"Stop downloading a file"
      ~handler:file_download_cancel
  ]
  |> String.Table.create_with_key_exn ~get_key:Command.name
;;

(* Fallback command *)
let fallback cmd =
  if String.is_prefix ~prefix:cmd "help"
  then
    commands
    |> String.Table.data
    |> List.sort ~compare:(fun a b -> String.compare (Command.name a) (Command.name b))
    |> List.map ~f:Command.description
    |> List.iter ~f:(rprintf [] "%s")
  else rprintf [] !"Unknown command: %{Style#prominent}\n" cmd
;;

(* Entry point *)
let exec client state cmd =
  match Parser.parse cmd with
  | Ok (command :: args) when String.Table.mem commands command ->
    let command = String.Table.find_exn commands command in
    (command.handler client state args)
  | Ok [] -> state
  | Ok (command :: _) ->
    fallback command;
    state
  | Error error ->
    rprintf [] "%s\n" error;
    state
;;
