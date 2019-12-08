open Core
open Tdlib.Models

module Update = struct
  let prefix chat user =
    if Chat.is_user_id chat (User.id user)
    then Chat.title chat
    else [ User.full_name user; Chat.title chat ] |> String.concat ~sep:" -> "
  ;;
end

module Message = struct
  open Message

  let display_content_of_text text = Formatted_text.text (Content.Text.text text)

  let display_content_of_file typ caption file =
    let local_file_path = file |> File.local |> File.Local.path in
    let info =
      [ caption; local_file_path ]
      |> List.filter ~f:(Fun.negate String.is_empty)
      |> String.concat ~sep:" -> "
    in
    if String.is_empty info
    then sprintf "[%s %ld]" typ (File.id file)
    else sprintf "[%s %ld] <%s>" typ (File.id file) info
  ;;

  let display_content_of_photo photo_content =
    let photo = Content.Photo.photo photo_content in
    if List.is_empty (Photo.sizes photo)
    then "[Empty photo]"
    else (
      let file = Option.value_exn (Photo.best_file photo) in
      display_content_of_file "Photo" (Content.Photo.caption photo_content) file)
  ;;

  let display_content_of_animation animation_content =
    let caption = animation_content |> Content.Animation.caption |> Formatted_text.text in
    let file = animation_content |> Content.Animation.animation |> Animation.animation in
    display_content_of_file "Animation" caption file
  ;;

  let display_content_of_audio audio_content =
    let caption = audio_content |> Content.Audio.caption in
    let file = audio_content |> Content.Audio.audio |> Audio.audio in
    display_content_of_file "Audio" caption file
  ;;

  let display_content_of_location location_content =
    let location = Content.Location.location location_content in
    let coordinates =
      sprintf "%.6f,%.6f" (Location.latitude location) (Location.longitude location)
    in
    let uri = Uri.of_string "https://www.google.com/maps/search/?api=1" in
    let uri = Uri.add_query_param' uri ("query", coordinates) in
    sprintf "[Location %s] <%s>" coordinates (Uri.to_string uri)
  ;;

  let display_content_of_voice_note voice_note_content =
    let caption = voice_note_content |> Content.Voice_note.caption in
    let file = voice_note_content |> Content.Voice_note.voice_note |> Voice_note.voice in
    display_content_of_file "Voice note" caption file
  ;;

  let display_content_of_change_photo content =
    let photo = Content.Change_photo.photo content in
    if List.is_empty (Photo.sizes photo)
    then "[Photo changed to empty photo]"
    else (
      let file = Option.value_exn (Photo.best_file photo) in
      display_content_of_file "Photo changed" "" file)
  ;;

  let display_content_of_change_title content =
    let title = Content.Change_title.title content in
    sprintf "[Title changed to %s]" title
  ;;

  let display_content_of_add_members content =
    sprintf
      "[%d new members added]"
      (List.length (Content.Add_members.member_user_ids content))
  ;;

  let display_content content =
    let open Content in
    match content with
    | Text text -> display_content_of_text text
    | Photo photo -> display_content_of_photo photo
    | Animation animation -> display_content_of_animation animation
    | Audio audio -> display_content_of_audio audio
    | Location location -> display_content_of_location location
    | Change_photo photo -> display_content_of_change_photo photo
    | Change_title title -> display_content_of_change_title title
    | Add_members members -> display_content_of_add_members members
    | Voice_note note -> display_content_of_voice_note note
    | Other _ -> "[Unsupported]"
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
      | `Duplicate -> None
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

    let escape =
      String.Escaping.escape ~escapeworthy:[ ' ' ] ~escape_char:'\\' |> Staged.unstage
    ;;

    let to_string file =
      [ (let path = escape (path file) in
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
