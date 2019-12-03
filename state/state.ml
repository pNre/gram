open Core
open Tdlib.Models

module Map (K : Map.S) (V : T) = struct
  include K

  type t = (Key.t, V.t, Key.comparator_witness) Base.Map.t
end

module Users = Map (Int32.Map) (User)
module Users_status = Map (Int32.Map) (User.Status)
module Chats = Map (Int64.Map) (Chat)

module Chat_action_key = struct
  include Tuple.Make (Int32) (Int64)
  include Tuple.Comparable (Int32) (Int64)
end

module Chat_actions = Map (Chat_action_key.Map) (Chat.Action)

type t =
  { is_ready : unit Async.Ivar.t
  ; users : Users.t
  ; users_status : Users_status.t
  ; chats : Chats.t
  ; chat_ids : int64 list
  ; chat_actions : Chat_actions.t
  ; unread_messages : Message.t list
  ; updated_message_ids : Int64.Set.t
  }

let empty =
  { is_ready = Async.Ivar.create ()
  ; users = Users.empty
  ; users_status = Users_status.empty
  ; chats = Chats.empty
  ; chat_ids = []
  ; chat_actions = Chat_actions.empty
  ; unread_messages = []
  ; updated_message_ids = Int64.Set.empty
  }
;;

(*Updated messages*)
let append_updated_message_id state id =
  { state with updated_message_ids = Int64.Set.add state.updated_message_ids id }
;;

let remove_updated_message_id state id =
  { state with updated_message_ids = Int64.Set.remove state.updated_message_ids id }
;;

let is_message_id_updated state id = Int64.Set.mem state.updated_message_ids id

(*Updated messages*)
let append_unread_message state message =
  { state with unread_messages = message :: state.unread_messages }
;;

let set_unread_messages state unread_messages = { state with unread_messages }

(*Events*)
let when_ready state = Async.Ivar.read state.is_ready

(*Users*)
let set_user state user =
  { state with users = Users.set state.users ~key:(User.id user) ~data:user }
;;

let user state id = Users.find state.users id

let users_by_name state name =
  Users.to_alist state.users
  |> List.map ~f:snd
  |> List.filter ~f:String.Caseless.(fun u -> User.full_name u = name)
;;

let lookup_users state q =
  let regexp = Str.regexp_case_fold ("^" ^ q) in
  Users.to_alist state.users
  |> List.map ~f:snd
  |> List.filter ~f:(fun user ->
         let full_name = User.full_name user in
         Str.string_match regexp full_name 0)
;;

(*Users status*)
let update_user_status state user new_status =
  let key = User.id user in
  match Users_status.find state.users_status key with
  | Some status when status = new_status -> `Duplicate
  | Some _ | None ->
    let users_status = Users_status.set state.users_status ~key ~data:new_status in
    `Ok { state with users_status }
;;

(*Chats*)
let set_chat_ids state chat_ids = { state with chat_ids }

let set_chat state chat =
  { state with chats = Chats.set state.chats ~key:(Chat.id chat) ~data:chat }
;;

let chat state id = Chats.find state.chats id

let lookup_chats state q =
  let regexp = Str.regexp_case_fold ("^" ^ q) in
  Chats.to_alist state.chats
  |> List.map ~f:snd
  |> List.filter ~f:(fun chat -> Str.string_match regexp (Chat.title chat) 0)
;;

let find_chat state q =
  let chats = Chats.to_alist state.chats |> List.map ~f:snd in
  let users = users_by_name state q in
  let user_ids = List.map users ~f:User.id in
  List.find chats ~f:(fun chat ->
      let matches_user =
        match Chat.typ chat with
        | Private private_chat
          when List.mem user_ids private_chat.user_id ~equal:Int32.( = ) -> true
        | _ -> false
      in
      let matches_title = String.Caseless.equal (Chat.title chat) q in
      matches_user || matches_title)
;;

let update_chat_action state new_action user chat =
  let key = User.id user, Chat.id chat in
  let action = Chat_actions.find state.chat_actions key in
  match action with
  | Some action when new_action = action -> `Duplicate, state
  | Some action when new_action = Cancel ->
    let chat_actions = Chat_actions.remove state.chat_actions key in
    let state = { state with chat_actions } in
    `Unset action, state
  | Some _ | None ->
    let chat_actions = Chat_actions.set state.chat_actions ~key ~data:new_action in
    let state = { state with chat_actions } in
    `Set new_action, state
;;
