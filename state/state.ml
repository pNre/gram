open Core
open Tdlib.Models

module Map (K : Map.S) (V : T) = struct
  include K

  type t = (Key.t, V.t, Key.comparator_witness) Base.Map.t
end

module Users = Map (Int32.Map) (User)
module Chats = Map (Int64.Map) (Chat)

type t =
  { is_ready : unit Async.Ivar.t
  ; users : Users.t
  ; chats : Chats.t
  ; chat_ids : int64 list
  ; unread_messages : Message.t list
  }

let empty =
  { is_ready = Async.Ivar.create ()
  ; users = Users.empty
  ; chats = Chats.empty
  ; chat_ids = []
  ; unread_messages = []
  }
;;

(*Setters*)
let set_user state (user : User.t) =
  { state with users = Users.set state.users ~key:user.id ~data:user }
;;

let set_chat_ids state chat_ids = { state with chat_ids }

let set_chat state (chat : Chat.t) =
  { state with chats = Chats.set state.chats ~key:(Chat.id chat) ~data:chat }
;;

let append_unread_message state message =
  { state with unread_messages = message :: state.unread_messages }
;;

let set_unread_messages state unread_messages = { state with unread_messages }

(*Events*)
let when_ready state = Async.Ivar.read state.is_ready

(*
 * Users
 *)
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

(*
 * Chats
 *)
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
  let user_ids = List.map users ~f:(fun { id; _ } -> id) in
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

let name_of_sender message state =
  let chat_id = Message.chat_id message in
  let sender_user_id = Message.sender_user_id message in
  if sender_user_id > 0l
  then (
    let user = Users.find state.users sender_user_id in
    Option.map user ~f:User.full_name)
  else (
    let chat = Chats.find state.chats chat_id in
    Option.map chat ~f:Chat.title)
;;
