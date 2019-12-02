open Core
open Tdlib.Models

module Update = struct
  let prefix chat user =
    let chat_user_id = Option.bind ~f:Chat.user_id chat in
    if chat_user_id = Option.map ~f:User.id user
    then Chat.title (Option.value_exn chat)
    else (
      let user_name = Option.map ~f:User.full_name user in
      let chat_title = Option.map chat ~f:Chat.title in
      [ user_name; chat_title ] |> List.filter_opt |> String.concat ~sep:" @ ")
  ;;
end

module Message = struct
  let display_title message chat_by_id user_by_id =
    let chat_id = Message.chat_id message in
    let sender_user_id = Message.sender_user_id message in
    Update.prefix (chat_by_id chat_id) (user_by_id sender_user_id)
  ;;

  let display_content message =
    match Message.content message with
    | Message_text text -> Some (Formatted_text.text (Message.Content.Text.text text))
    | Message_photo { caption = { text = caption; _ }; photo = { sizes; _ }; _ } ->
      let or_none s = if String.length s = 0 then None else Some s in
      let caption = or_none caption in
      let file =
        sizes
        |> List.last
        |> Option.map ~f:(fun photo ->
               let file = Photo.Size.photo photo in
               let id = File.id file in
               let local_file_path = file |> File.local |> File.Local.path in
               Option.value ~default:(Int32.to_string id) (or_none local_file_path))
      in
      let info = String.concat ~sep:" @ " (List.filter_opt [ caption; file ]) in
      Some (sprintf "[Photo] <%s>" info)
    | Message_animation _ -> Some "[Animation]"
    | Other _ -> None
  ;;

  let to_description_string message chat_by_id user_by_id =
    let title = display_title message chat_by_id user_by_id in
    let content = display_content message in
    Option.map content ~f:(fun content -> sprintf "%s: %s\n" title content)
  ;;
end

module Chat = struct
  module Action = struct
    open Chat.Action

    let to_description_string = function
      | Cancel -> "Cancelled previous action"
      | Choosing_contact -> "Choosing a contact"
      | Choosing_location -> "Choosing a location"
      | Recording_video -> "Recording a video"
      | Recording_video_note -> "Recording a video note"
      | Recording_voice_note -> "Recording a voice note"
      | Start_playing_game -> "Playing a game"
      | Typing -> "Typing"
      | Uploading_document -> "Uploading a document"
      | Uploading_photo -> "Uploading a photo"
      | Uploading_video -> "Uploading a video"
      | Uploading_video_note -> "Uploading a video note"
      | Uploading_voice_note -> "Uploading a voice note"
      | Other _ -> "Unknown action"
    ;;
  end
end

module User = struct
  module Status = struct
    open User.Status

    let to_description_string = function
      | Online -> Some "Online"
      | Offline -> Some "Offline"
      | Recently -> Some "Seen recently"
      | Last_week -> Some "Seen last week"
      | Last_month -> Some "Seen last month"
      | Empty | Other _ -> None
    ;;
  end
end

module File = struct
  module Local = struct
    open File.Local

    let to_description_string file =
      [ (let path = Str.global_replace (Str.regexp " ") "\\ " (path file) in
         if String.length path > 0 then Some (path ^ " ->") else None)
      ; (if is_downloading_active file
        then Some "Downloading"
        else if is_downloading_completed file
        then Some "Downloaded"
        else None)
      ]
      |> List.filter_opt
      |> String.concat ~sep:" "
    ;;
  end
end
