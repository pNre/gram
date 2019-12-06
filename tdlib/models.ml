open Core
open Yojson.Safe.Util

module Time = struct
  include Time

  let t_of_yojson j =
    j |> int64_of_yojson |> Int64.to_float |> Time.Span.of_sec |> Time.of_span_since_epoch
  ;;

  let yojson_of_t t =
    t |> Time.to_span_since_epoch |> Time.Span.to_sec |> Int64.of_float |> yojson_of_int64
  ;;
end

module Order = struct
  include Int64

  let t_of_yojson j =
    match j with
    | `String s -> Int64.of_string s
    | _ -> int64_of_yojson j
  ;;

  let yojson_of_t = yojson_of_int64
end

module Tdlib = struct
  module Parameters = struct
    type t =
      { database_directory : string
      ; use_message_database : bool
      ; use_secret_chats : bool
      ; api_id : string
      ; api_hash : string
      ; system_language_code : string
      ; device_model : string
      ; system_version : string
      ; application_version : string
      ; enable_storage_optimizer : bool
      }
    [@@deriving yojson, fields]

    let create = Fields.create
  end
end

module Type_field = struct
  let t_of_yojson j = to_string (member "@type" j)
  let yojson_of_t t = `Assoc [ "@type", `String t ]
end

module Option_value = struct
  type t =
    | Empty
    | Boolean of bool
    | Integer of int
    | String of string
    | Other of Yojson.Safe.t

  let value v = `Assoc [ "value", v ]

  let yojson_of_t = function
    | Empty -> Type_field.yojson_of_t "optionValueEmpty"
    | Boolean bool ->
      combine (Type_field.yojson_of_t "optionValueBoolean") (value (`Bool bool))
    | Integer int ->
      combine (Type_field.yojson_of_t "optionValueInteger") (value (`Int int))
    | String string ->
      combine (Type_field.yojson_of_t "optionValueString") (value (`String string))
    | Other j -> j
  ;;

  let t_of_yojson j =
    match Type_field.t_of_yojson j with
    | "optionValueEmpty" -> Empty
    | "optionValueBoolean" -> Boolean (to_bool (member "value" j))
    | "optionValueInteger" -> Integer (to_int (member "value" j))
    | "optionValueString" -> String (to_string (member "value" j))
    | _ -> Other j
  ;;
end

type update_option =
  { name : string
  ; value : Option_value.t
  }
[@@deriving yojson]

module User = struct
  module Id = struct
    include Int32

    let t_of_yojson = int32_of_yojson
    let yojson_of_t = yojson_of_int32
  end

  module Status = struct
    type t =
      | Empty
      | Last_month
      | Last_week
      | Offline
      | Online
      | Recently
      | Other of Yojson.Safe.t

    let yojson_of_t = function
      | Empty -> Type_field.yojson_of_t "userStatusEmpty"
      | Last_month -> Type_field.yojson_of_t "userStatusLastMonth"
      | Last_week -> Type_field.yojson_of_t "userStatusLastWeek"
      | Offline -> Type_field.yojson_of_t "userStatusOffline"
      | Online -> Type_field.yojson_of_t "userStatusOnline"
      | Recently -> Type_field.yojson_of_t "userStatusRecently"
      | Other j -> j
    ;;

    let t_of_yojson j =
      match Type_field.t_of_yojson j with
      | "userStatusEmpty" -> Empty
      | "userStatusLastMonth" -> Last_month
      | "userStatusLastWeek" -> Last_week
      | "userStatusOffline" -> Offline
      | "userStatusOnline" -> Online
      | "userStatusRecently" -> Recently
      | _ -> Other j
    ;;
  end

  module Type = struct
    type t =
      | Bot
      | Deleted
      | Regular
      | Unknown
      | Other of Yojson.Safe.t

    let yojson_of_t = function
      | Bot -> Type_field.yojson_of_t "userTypeBot"
      | Deleted -> Type_field.yojson_of_t "userTypeDeleted"
      | Regular -> Type_field.yojson_of_t "userTypeRegular"
      | Unknown -> Type_field.yojson_of_t "userTypeUnknown"
      | Other j -> j
    ;;

    let t_of_yojson j =
      match Type_field.t_of_yojson j with
      | "userTypeBot" -> Bot
      | "userTypeDeleted" -> Deleted
      | "userTypeRegular" -> Regular
      | "userTypeUnknown" -> Unknown
      | _ -> Other j
    ;;
  end

  module Link_state = struct
    type t =
      | Is_contact
      | Knows_phone_number
      | None
      | Other of Yojson.Safe.t

    let yojson_of_t = function
      | Is_contact -> Type_field.yojson_of_t "linkStateIsContact"
      | Knows_phone_number -> Type_field.yojson_of_t "linkStateKnowsPhoneNumber"
      | None -> Type_field.yojson_of_t "linkStateNone"
      | Other j -> j
    ;;

    let t_of_yojson j =
      match Type_field.t_of_yojson j with
      | "linkStateIsContact" -> Is_contact
      | "linkStateKnowsPhoneNumber" -> Knows_phone_number
      | "linkStateNone" -> None
      | _ -> Other j
    ;;
  end

  module Request = struct
    module Update_status = struct
      type t =
        { user_id : Id.t
        ; status : Status.t
        }
      [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
    end
  end

  type t =
    { id : Id.t
    ; first_name : string
    ; last_name : string
    ; username : string
    ; phone_number : string
    ; status : Status.t
    ; outgoing_link : Link_state.t
    ; incoming_link : Link_state.t
    ; is_verified : bool
    ; is_support : bool
    ; restriction_reason : string
    ; is_scam : bool
    ; have_access : bool
    ; typ : Type.t [@key "type"]
    ; language_code : string
    }
  [@@deriving yojson, fields] [@@yojson.allow_extra_fields]

  let full_name user =
    Caml.String.trim (Printf.sprintf "%s %s" (first_name user) (last_name user))
  ;;
end

module Chat = struct
  module Id = struct
    include Int64

    let t_of_yojson = int64_of_yojson
    let yojson_of_t = yojson_of_int64
  end

  module Type = struct
    type basic_group = { basic_group_id : int32 }
    [@@deriving yojson] [@@yojson.allow_extra_fields]

    type private_ = { user_id : User.Id.t }
    [@@deriving yojson] [@@yojson.allow_extra_fields]

    type secret =
      { secret_chat_id : int32
      ; user_id : User.Id.t
      }
    [@@deriving yojson] [@@yojson.allow_extra_fields]

    type supergroup =
      { supergroup_id : int32
      ; is_channel : bool
      }
    [@@deriving yojson] [@@yojson.allow_extra_fields]

    type t =
      | Basic_group of basic_group
      | Private of private_
      | Secret of secret
      | Supergroup of supergroup
      | Other of Yojson.Safe.t

    let yojson_of_t = function
      | Basic_group group ->
        combine
          (Type_field.yojson_of_t "chatTypeBasicGroup")
          (yojson_of_basic_group group)
      | Private priv ->
        combine (Type_field.yojson_of_t "chatTypePrivate") (yojson_of_private_ priv)
      | Secret secret ->
        combine (Type_field.yojson_of_t "chatTypeSecret") (yojson_of_secret secret)
      | Supergroup group ->
        combine (Type_field.yojson_of_t "chatTypeSupergroup") (yojson_of_supergroup group)
      | Other j -> j
    ;;

    let t_of_yojson j =
      match Type_field.t_of_yojson j with
      | "chatTypeBasicGroup" -> Basic_group (basic_group_of_yojson j)
      | "chatTypePrivate" -> Private (private__of_yojson j)
      | "chatTypeSecret" -> Secret (secret_of_yojson j)
      | "chatTypeSupergroup" -> Supergroup (supergroup_of_yojson j)
      | _ -> Other j
    ;;
  end

  module Action = struct
    type t =
      | Cancel
      | Choosing_contact
      | Choosing_location
      | Recording_video
      | Recording_video_note
      | Recording_voice_note
      | Start_playing_game
      | Typing
      | Uploading_document
      | Uploading_photo
      | Uploading_video
      | Uploading_video_note
      | Uploading_voice_note
      | Other of Yojson.Safe.t

    let yojson_of_t = function
      | Cancel -> Type_field.yojson_of_t "chatActionCancel"
      | Choosing_contact -> Type_field.yojson_of_t "chatActionChoosingContact"
      | Choosing_location -> Type_field.yojson_of_t "chatActionChoosingLocation"
      | Recording_video -> Type_field.yojson_of_t "chatActionRecordingVideo"
      | Recording_video_note -> Type_field.yojson_of_t "chatActionRecordingVideoNote"
      | Recording_voice_note -> Type_field.yojson_of_t "chatActionRecordingVoiceNote"
      | Start_playing_game -> Type_field.yojson_of_t "chatActionStartPlayingGame"
      | Typing -> Type_field.yojson_of_t "chatActionTyping"
      | Uploading_document -> Type_field.yojson_of_t "chatActionUploadingDocument"
      | Uploading_photo -> Type_field.yojson_of_t "chatActionUploadingPhoto"
      | Uploading_video -> Type_field.yojson_of_t "chatActionUploadingVideo"
      | Uploading_video_note -> Type_field.yojson_of_t "chatActionUploadingVideoNote"
      | Uploading_voice_note -> Type_field.yojson_of_t "chatActionUploadingVoiceNote"
      | Other j -> j
    ;;

    let t_of_yojson j =
      match Type_field.t_of_yojson j with
      | "chatActionCancel" -> Cancel
      | "chatActionChoosingContact" -> Choosing_contact
      | "chatActionChoosingLocation" -> Choosing_location
      | "chatActionRecordingVideo" -> Recording_video
      | "chatActionRecordingVideoNote" -> Recording_video_note
      | "chatActionRecordingVoiceNote" -> Recording_voice_note
      | "chatActionStartPlayingGame" -> Start_playing_game
      | "chatActionTyping" -> Typing
      | "chatActionUploadingDocument" -> Uploading_document
      | "chatActionUploadingPhoto" -> Uploading_photo
      | "chatActionUploadingVideo" -> Uploading_video
      | "chatActionUploadingVideoNote" -> Uploading_video_note
      | "chatActionUploadingVoiceNote" -> Uploading_voice_note
      | _ -> Other j
    ;;
  end

  module Request = struct
    module Get_chats = struct
      type t =
        { offset_order : int64
        ; offset_chat_id : Id.t
        ; limit : int32
        }
      [@@deriving yojson, fields]

      let create
          ?(offset_order = Int64.max_value)
          ?(offset_chat_id = 0L)
          ?(limit = 100l)
          ()
        =
        Fields.create ~offset_order ~offset_chat_id ~limit
      ;;
    end

    module Update_action = struct
      type t =
        { chat_id : Id.t
        ; user_id : User.Id.t
        ; action : Action.t
        }
      [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
    end
  end

  module Read_inbox = struct
    type t =
      { chat_id : Id.t
      ; unread_count : int32
      }
    [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
  end

  type t =
    { id : Id.t
    ; title : string
    ; typ : Type.t [@key "type"]
    ; order : Order.t
    ; is_pinned : bool
    ; is_marked_as_unread : bool
    ; is_sponsored : bool
    ; can_be_deleted_only_for_self : bool
    ; can_be_deleted_for_all_users : bool
    ; unread_count : int32
    ; unread_mention_count : int32
    }
  [@@deriving yojson, fields] [@@yojson.allow_extra_fields]

  let user_id chat =
    match typ chat with
    | Basic_group _ | Supergroup _ | Other _ -> None
    | Private { user_id; _ } | Secret { user_id; _ } -> Some user_id
  ;;
end

module File = struct
  module Local = struct
    type t =
      { path : string
      ; can_be_downloaded : bool
      ; can_be_deleted : bool
      ; is_downloading_active : bool
      ; is_downloading_completed : bool
      ; download_offset : int32
      ; downloaded_prefix_size : int32
      ; downloaded_size : int32
      }
    [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
  end

  module Remote = struct
    type t =
      { id : string
      ; is_uploading_active : bool
      ; is_uploading_completed : bool
      ; uploaded_size : int32
      }
    [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
  end

  module Request = struct
    module Download = struct
      type t =
        { file_id : int32
        ; priority : int32
        ; offset : int32
        ; limit : int32
        ; synchronous : bool
        }
      [@@deriving yojson, fields] [@@yojson.allow_extra_fields]

      let create = Fields.create
    end
  end

  type t =
    { id : int32
    ; size : int32
    ; expected_size : int32
    ; local : Local.t
    ; remote : Remote.t
    }
  [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
end

module Photo = struct
  module Size = struct
    type t =
      { typ : string [@key "type"]
      ; photo : File.t
      ; width : int32
      ; height : int32
      }
    [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
  end

  type t =
    { has_stickers : bool
    ; sizes : Size.t list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Animation = struct
  type t =
    { duration : int32
    ; width : int32
    ; height : int32
    ; file_name : string
    ; mime_type : string
    ; animation : File.t
    }
  [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
end

module Formatted_text = struct
  type t = { text : string } [@@deriving yojson, fields] [@@yojson.allow_extra_fields]

  let create = Fields.create
end

module Message = struct
  module Id = struct
    include Int64

    let t_of_yojson = int64_of_yojson
    let yojson_of_t = yojson_of_int64
  end

  module Content = struct
    module Text = struct
      type t = { text : Formatted_text.t }
      [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
    end

    module Animation = struct
      type t =
        { is_secret : bool
        ; caption : Formatted_text.t
        ; animation : Animation.t
        }
      [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
    end

    module Photo = struct
      type t =
        { is_secret : bool
        ; caption : Formatted_text.t
        ; photo : Photo.t
        }
      [@@deriving yojson] [@@yojson.allow_extra_fields]

      let best_file { photo = { sizes; _ }; _ } =
        sizes |> List.last |> Option.map ~f:Photo.Size.photo
      ;;

      let caption { caption; _ } = Formatted_text.text caption
    end

    module Input = struct
      module Text = struct
        type t =
          { text : Formatted_text.t
          ; disable_web_page_preview : bool
          ; clear_draft : bool
          }
        [@@deriving yojson, fields] [@@yojson.allow_extra_fields]

        let create ?(disable_web_page_preview = false) ?(clear_draft = false) text =
          Fields.create ~text ~disable_web_page_preview ~clear_draft
        ;;
      end

      module File = struct
        type t =
          | Local of string
          | Other of Yojson.Safe.t

        let yojson_of_t = function
          | Local path ->
            combine
              (Type_field.yojson_of_t "inputFileLocal")
              (`Assoc [ "path", `String path ])
          | Other j -> j
        ;;

        let t_of_yojson j =
          match Type_field.t_of_yojson j with
          | "inputFileLocal" -> Local (to_string (member "path" j))
          | _ -> Other j
        ;;
      end

      module Photo = struct
        type t =
          { photo : File.t
          ; added_sticker_file_ids : int32 list
          ; width : int32
          ; height : int32
          ; caption : Formatted_text.t
          ; ttl : int32
          }
        [@@deriving yojson, fields] [@@yojson.allow_extra_fields]

        let create
            ?(added_sticker_file_ids = [])
            ?(width = 0l)
            ?(height = 0l)
            ?(ttl = 0l)
            caption
            photo
          =
          Fields.create ~photo ~added_sticker_file_ids ~width ~height ~caption ~ttl
        ;;
      end

      type t =
        | Input_message_text of Text.t
        | Input_message_photo of Photo.t
        | Other of Yojson.Safe.t

      let yojson_of_t = function
        | Input_message_text text ->
          combine (Type_field.yojson_of_t "inputMessageText") (Text.yojson_of_t text)
        | Input_message_photo photo ->
          combine (Type_field.yojson_of_t "inputMessagePhoto") (Photo.yojson_of_t photo)
        | Other j -> j
      ;;

      let t_of_yojson j =
        match Type_field.t_of_yojson j with
        | "inputMessageText" -> Input_message_text (Text.t_of_yojson j)
        | "inputMessagePhoto" -> Input_message_photo (Photo.t_of_yojson j)
        | _ -> Other j
      ;;
    end

    type t =
      | Message_text of Text.t
      | Message_photo of Photo.t
      | Message_animation of Animation.t
      | Other of Yojson.Safe.t

    let yojson_of_t = function
      | Message_text text ->
        combine (Type_field.yojson_of_t "messageText") (Text.yojson_of_t text)
      | Message_photo photo ->
        combine (Type_field.yojson_of_t "messagePhoto") (Photo.yojson_of_t photo)
      | Message_animation animation ->
        combine
          (Type_field.yojson_of_t "messageAnimation")
          (Animation.yojson_of_t animation)
      | Other j -> j
    ;;

    let t_of_yojson j =
      match Type_field.t_of_yojson j with
      | "messageText" -> Message_text (Text.t_of_yojson j)
      | "messagePhoto" -> Message_photo (Photo.t_of_yojson j)
      | "messageAnimation" -> Message_animation (Animation.t_of_yojson j)
      | _ -> Other j
    ;;
  end

  module Request = struct
    module Get = struct
      type t =
        { chat_id : Chat.Id.t
        ; message_id : Id.t
        }
      [@@deriving yojson, fields]

      let create = Fields.create
    end

    module View = struct
      type t =
        { chat_id : Chat.Id.t
        ; message_ids : Id.t list
        ; force_read : bool
        }
      [@@deriving yojson, fields] [@@yojson.allow_extra_fields]

      let create = Fields.create
    end

    module Send = struct
      type t =
        { chat_id : Chat.Id.t
        ; reply_to_message_id : Id.t
        ; disable_notification : bool
        ; from_background : bool
        ; input_message_content : Content.Input.t
        }
      [@@deriving yojson, fields] [@@yojson.allow_extra_fields]

      let create
          ?(reply_to_message_id = 0L)
          ?(disable_notification = false)
          ?(from_background = false)
          chat_id
          input_message_content
        =
        Fields.create
          ~reply_to_message_id
          ~disable_notification
          ~from_background
          ~chat_id
          ~input_message_content
      ;;
    end

    module Update_content = struct
      type t =
        { chat_id : Chat.Id.t
        ; message_id : Id.t
        ; new_content : Content.t
        }
      [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
    end
  end

  type t =
    { id : int64
    ; sender_user_id : User.Id.t
    ; chat_id : Chat.Id.t
    ; is_outgoing : bool
    ; can_be_edited : bool
    ; can_be_forwarded : bool
    ; can_be_deleted_only_for_self : bool
    ; can_be_deleted_for_all_users : bool
    ; is_channel_post : bool
    ; contains_unread_mention : bool
    ; date : Time.t
    ; edit_date : Time.t
    ; reply_to_message_id : int64
    ; ttl : int32
    ; ttl_expires_in : float
    ; via_bot_user_id : int
    ; author_signature : string
    ; views : int
    ; content : Content.t
    }
  [@@deriving yojson, fields] [@@yojson.allow_extra_fields]
end

module Log = struct
  module Verbosity_level = struct
    type t =
      | Fatal
      | Error
      | Warning
      | Info
      | Other of int

    let of_int = function
      | 0 -> Fatal
      | 1 -> Error
      | 2 -> Warning
      | 3 -> Info
      | n -> Other n
    ;;

    let to_int = function
      | Fatal -> 0
      | Error -> 1
      | Warning -> 2
      | Info -> 3
      | Other n -> n
    ;;

    let yojson_of_t t = `Int (to_int t)
    let t_of_yojson j = of_int (Yojson.Safe.Util.to_int j)
  end
end

module Authorization_state = struct
  type t =
    | Wait_tdlib_parameters
    | Wait_encryption_key
    | Wait_phone_number
    | Wait_code
    | Ready
    | Other of Yojson.Safe.t

  let yojson_of_t = function
    | Wait_tdlib_parameters ->
      Type_field.yojson_of_t "authorizationStateWaitTdlibParameters"
    | Wait_encryption_key -> Type_field.yojson_of_t "authorizationStateWaitEncryptionKey"
    | Wait_phone_number -> Type_field.yojson_of_t "authorizationStateWaitPhoneNumber"
    | Wait_code -> Type_field.yojson_of_t "authorizationStateWaitCode"
    | Ready -> Type_field.yojson_of_t "authorizationStateReady"
    | Other j -> j
  ;;

  let t_of_yojson j =
    match Type_field.t_of_yojson j with
    | "authorizationStateWaitTdlibParameters" -> Wait_tdlib_parameters
    | "authorizationStateWaitEncryptionKey" -> Wait_encryption_key
    | "authorizationStateWaitPhoneNumber" -> Wait_phone_number
    | "authorizationStateWaitCode" -> Wait_code
    | "authorizationStateReady" -> Ready
    | _ -> Other j
  ;;
end

module Request = struct
  type t =
    | Set_log_verbosity_level of Log.Verbosity_level.t
    | Set_tdlib_parameters of Tdlib.Parameters.t
    | Set_authentication_phone_number of string
    | Check_database_encryption_key of string
    | Check_authentication_code of string
    | Get_contacts
    | Get_chat of Chat.Id.t
    | Get_chats of Chat.Request.Get_chats.t
    | Get_message of Message.Request.Get.t
    | Chats of Chat.Id.t list
    | Chat of Chat.t
    | Message of Message.t
    | Send_message of Message.Request.Send.t
    | View_messages of Message.Request.View.t
    | Cancel_download_file of int32
    | Download_file of File.Request.Download.t
    | Update_authorization_state of Authorization_state.t
    | Update_new_message of Message.t
    | Update_message_content of Message.Request.Update_content.t
    | Update_new_chat of Chat.t
    | Update_chat_read_inbox of Chat.Read_inbox.t
    | Update_user of User.t
    | Update_user_status of User.Request.Update_status.t
    | Update_user_chat_action of Chat.Request.Update_action.t
    | Update_file of File.t
    | Other of Yojson.Safe.t

  let create typ p = combine (Type_field.yojson_of_t typ) p
  let create' typ prop p = create typ (`Assoc [ prop, p ])

  let yojson_of_t = function
    | Set_log_verbosity_level new_verbosity_level ->
      create'
        "setLogVerbosityLevel"
        "new_verbosity_level"
        (Log.Verbosity_level.yojson_of_t new_verbosity_level)
    | Set_tdlib_parameters parameters ->
      create' "setTdlibParameters" "parameters" (Tdlib.Parameters.yojson_of_t parameters)
    | Set_authentication_phone_number phone_number ->
      create' "setAuthenticationPhoneNumber" "phone_number" (`String phone_number)
    | Check_database_encryption_key key ->
      create' "checkDatabaseEncryptionKey" "key" (`String key)
    | Check_authentication_code code ->
      create' "checkAuthenticationCode" "code" (`String code)
    | Get_contacts -> Type_field.yojson_of_t "getContacts"
    | Get_chat chat_id -> create' "getChat" "chat_id" (Chat.Id.yojson_of_t chat_id)
    | Get_chats request -> create "getChats" (Chat.Request.Get_chats.yojson_of_t request)
    | Get_message request -> create "getMessage" (Message.Request.Get.yojson_of_t request)
    | Chats ids ->
      create' "chats" "chat_ids" (`List (List.map ids ~f:Chat.Id.yojson_of_t))
    | Chat chat -> create "chat" (Chat.yojson_of_t chat)
    | Message message -> create "message" (Message.yojson_of_t message)
    | Send_message request ->
      create "sendMessage" (Message.Request.Send.yojson_of_t request)
    | View_messages request ->
      create "viewMessages" (Message.Request.View.yojson_of_t request)
    | Cancel_download_file file_id ->
      create' "cancelDownloadFile" "file_id" (yojson_of_int32 file_id)
    | Download_file request ->
      create "downloadFile" (File.Request.Download.yojson_of_t request)
    | Update_authorization_state state ->
      create'
        "updateAuthorizationState"
        "authorization_state"
        (Authorization_state.yojson_of_t state)
    | Update_new_message message ->
      create' "updateNewMessage" "message" (Message.yojson_of_t message)
    | Update_message_content request ->
      create "updateMessageContent" (Message.Request.Update_content.yojson_of_t request)
    | Update_new_chat chat -> create' "updateNewChat" "chat" (Chat.yojson_of_t chat)
    | Update_chat_read_inbox read_inbox ->
      create "updateChatReadInbox" (Chat.Read_inbox.yojson_of_t read_inbox)
    | Update_user user -> create' "updateUser" "user" (User.yojson_of_t user)
    | Update_user_status status ->
      create "updateUserStatus" (User.Request.Update_status.yojson_of_t status)
    | Update_user_chat_action action ->
      create "updateUserChatAction" (Chat.Request.Update_action.yojson_of_t action)
    | Update_file file -> create' "updateFile" "file" (File.yojson_of_t file)
    | Other j -> j
  ;;

  let t_of_yojson j =
    match Type_field.t_of_yojson j with
    | "setLogVerbosityLevel" ->
      Set_log_verbosity_level
        (Log.Verbosity_level.t_of_yojson (member "new_verbosity_level" j))
    | "setTdlibParameters" ->
      Set_tdlib_parameters (Tdlib.Parameters.t_of_yojson (member "parameters" j))
    | "setAuthenticationPhoneNumber" ->
      Set_authentication_phone_number (to_string (member "phone_number" j))
    | "checkDatabaseEncryptionKey" ->
      Check_database_encryption_key (to_string (member "key" j))
    | "checkAuthenticationCode" -> Check_authentication_code (to_string (member "code" j))
    | "getContacts" -> Get_contacts
    | "getChat" -> Get_chat (Chat.Id.t_of_yojson (member "chat_id" j))
    | "getChats" -> Get_chats (Chat.Request.Get_chats.t_of_yojson j)
    | "getMessage" -> Get_message (Message.Request.Get.t_of_yojson j)
    | "chats" -> Chats (list_of_yojson Chat.Id.t_of_yojson (member "chat_ids" j))
    | "chat" -> Chat (Chat.t_of_yojson j)
    | "message" -> Message (Message.t_of_yojson j)
    | "sendMessage" -> Send_message (Message.Request.Send.t_of_yojson j)
    | "viewMessages" -> View_messages (Message.Request.View.t_of_yojson j)
    | "cancelDownloadFile" -> Cancel_download_file (int32_of_yojson (member "file_id" j))
    | "downloadFile" -> Download_file (File.Request.Download.t_of_yojson j)
    | "updateAuthorizationState" ->
      Update_authorization_state
        (Authorization_state.t_of_yojson (member "authorization_state" j))
    | "updateNewMessage" -> Update_new_message (Message.t_of_yojson (member "message" j))
    | "updateMessageContent" ->
      Update_message_content (Message.Request.Update_content.t_of_yojson j)
    | "updateNewChat" -> Update_new_chat (Chat.t_of_yojson (member "chat" j))
    | "updateChatReadInbox" -> Update_chat_read_inbox (Chat.Read_inbox.t_of_yojson j)
    | "updateUser" -> Update_user (User.t_of_yojson (member "user" j))
    | "updateUserStatus" -> Update_user_status (User.Request.Update_status.t_of_yojson j)
    | "updateUserChatAction" ->
      Update_user_chat_action (Chat.Request.Update_action.t_of_yojson j)
    | "updateFile" -> Update_file (File.t_of_yojson (member "file" j))
    | _ -> Other j
  ;;
end
