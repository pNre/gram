open Async
open Core
open Console.Ansi
open Tdlib

module Parser = struct
  open Angstrom
  open Char

  let escape = '\\'
  let quote = '"'
  let is_identifier c = (not (is_whitespace c)) && not (c = quote)
  let identifier = take_while1 is_identifier <?> "identifier"
  let whitespaces = take_while is_whitespace <?> "whitespaces"
  let escaped_space = char escape *> satisfy is_whitespace

  let string_literal =
    let quoted = char quote *> many (not_char quote) <* char quote <?> "quoted string" in
    let escaped = many1 (escaped_space <|> satisfy is_identifier) <?> "escaped string" in
    quoted <|> escaped >>| String.of_char_list
  ;;

  let parse =
    parse_string
      (many_till
         (whitespaces *> string_literal <|> identifier <* whitespaces)
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
      [ Some (string_with_attr [ `Bright ] (name arg)); summary arg ]
      |> List.filter_opt
      |> String.concat ~sep
    ;;
  end

  type t =
    { name : string
    ; shape : Arg.t list
    ; summary : string
    ; handler : Client.t -> State.t -> string list -> unit Deferred.t
    }
  [@@deriving fields]

  let create = Fields.create
  let arg ~name ~summary = Arg.Fields.create ~name ~summary:(Some summary)
  let anon = Arg.Fields.create ~summary:None

  let description command =
    sprintf
      "\t%s\n\t    %s\n\t    - %s\n\n"
      (string_with_attr [ `Bright ] (name command))
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
        let input_message_content =
          Input.Input_message_text
            (Input.Text.create (Models.Formatted_text.create ~text:message))
        in
        let req = Models.Message.Request.Send.create chat_id input_message_content in
        Client.send client (Send_message req))
  in
  match args with
  | recipient :: args -> return (send_message recipient (String.concat ~sep:" " args))
  | _ -> return ()
;;

(*Sends a photo*)
let photo client state args =
  let send_photo recipient caption path =
    let chat = State.find_chat state recipient in
    Option.iter chat ~f:(fun chat ->
        let open Models.Message.Content in
        let chat_id = Models.Chat.id chat in
        let input_message_content =
          Input.Input_message_photo
            (Input.Photo.create
               (Models.Formatted_text.create ~text:caption)
               (Input.File.Local path))
        in
        let req = Models.Message.Request.Send.create chat_id input_message_content in
        Client.send client (Send_message req))
  in
  match args with
  | recipient :: caption :: path :: _ -> return (send_photo recipient caption path)
  | _ -> return ()
;;

(* Marks a chat as read *)
let read client state args =
  let q = String.concat ~sep:" " args in
  let chat = State.find_chat state q in
  return
    (Option.iter chat ~f:(fun chat ->
         let message_ids = List.map state.unread_messages ~f:Models.Message.id in
         let req =
           Models.Message.Request.View.create
             ~chat_id:(Models.Chat.id chat)
             ~message_ids
             ~force_read:true
         in
         Client.send client (View_messages req)))
;;

(* Downloads a file *)
let file_download client _state args =
  return
    (args
    |> List.hd
    |> Option.map ~f:Int32.of_string
    |> Option.iter ~f:(fun file_id ->
           let req =
             Models.File.Request.Download.create
               ~file_id
               ~priority:1l
               ~offset:0l
               ~limit:0l
               ~synchronous:true
           in
           Client.send client (Models.Request.Download_file req)))
;;

(* Stops downloading a file *)
let file_download_cancel client _state args =
  return
    (args
    |> List.hd
    |> Option.map ~f:Int32.of_string
    |> Option.iter ~f:(fun file_id ->
           Client.send client (Models.Request.Cancel_download_file file_id)))
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
    |> List.iter ~f:(printf [] "%s")
  else printf [] "Unknown command: %s\n" (string_with_attr [ `Bright ] cmd)
;;

(* Entry point *)
let exec client state cmd =
  match Parser.parse cmd with
  | Ok (command :: args) when String.Table.mem commands command ->
    let command = String.Table.find_exn commands command in
    command.handler client state args
  | Ok [] -> return ()
  | Ok (command :: _) -> return (fallback command)
  | Error error ->
    printf [] "%s\n" error;
    return ()
;;
