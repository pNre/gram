open Core
open Tdlib.Models

module Update = struct
  let prefix chat user =
    let chat_user_id = Option.bind chat ~f:Chat.user_id in
    if chat_user_id = Option.map user ~f:User.id
    then Chat.title (Option.value_exn chat)
    else (
      let user_name = Option.map user ~f:User.full_name in
      let chat_title = Option.map chat ~f:Chat.title in
      [ user_name; chat_title ] |> List.filter_opt |> String.concat ~sep:" @ ")
  ;;
end

module Message = struct
  open Message.Content

  let display_content_of_photo photo_content =
    let open Photo in
    let file = Option.value_exn (best_file photo_content) in
    let local_file_path = file |> File.local |> File.Local.path in
    let info =
      [ caption photo_content; local_file_path ]
      |> List.filter ~f:(Fun.negate String.is_empty)
      |> String.concat ~sep:" -> "
    in
    if String.is_empty info
    then sprintf "[Photo %ld]" (File.id file)
    else sprintf "[Photo %ld] <%s>" (File.id file) info
  ;;

  let display_content = function
    | Message_text text -> Some (Formatted_text.text (Text.text text))
    | Message_photo photo when not (List.is_empty photo.photo.sizes) ->
      Some (display_content_of_photo photo)
    | Message_photo _ -> None
    | Message_animation _ -> Some "[Animation]"
    | Other _ -> None
  ;;

  let to_description_string chat user content =
    let title = Update.prefix chat user in
    let content = display_content content in
    Option.map content ~f:(fun content -> sprintf "%s: %s\n" title content)
  ;;
end

module Chat = struct
  module Action = struct
    open Chat.Action

    let to_string = function
      | Choosing_contact -> Some "Choosing a contact"
      | Choosing_location -> Some "Choosing a location"
      | Recording_video -> Some "Recording a video"
      | Recording_video_note -> Some "Recording a video note"
      | Recording_voice_note -> Some "Recording a voice note"
      | Start_playing_game -> Some "Playing a game"
      | Typing -> Some "Typing"
      | Uploading_document -> Some "Uploading a document"
      | Uploading_photo -> Some "Uploading a photo"
      | Uploading_video -> Some "Uploading a video"
      | Uploading_video_note -> Some "Uploading a video note"
      | Uploading_voice_note -> Some "Uploading a voice note"
      | Cancel | Other _ -> None
    ;;

    let to_description_string = function
      | `Set action -> to_string action
      | `Unset action ->
        Option.map (to_string action) ~f:(fun action -> "(Stopped) " ^ action)
      | `No_op -> None
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
