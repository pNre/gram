open Core
open Core.Poly
open Tdlib.Models

module Map (K : Map.S) (V : T) = struct
  include K

  type t = (Key.t, V.t, Key.comparator_witness) Map.t
end

module Message_id_list = struct
  type t = Message.Id.t list
end

module Users = Map (User.Id.Map) (User)
module Users_status = Map (User.Id.Map) (User.Status)
module Chats = Map (Chat.Id.Map) (Chat)
module User_id_chat_id = Tuple.Comparable (User.Id) (Chat.Id)
module Unread_messages = Map (Chat.Id.Map) (Message_id_list)
module Chat_actions = Map (User_id_chat_id.Map) (Chat.Action)

type t =
  { is_ready : unit Async.Ivar.t
  ; users : Users.t
  ; users_status : Users_status.t
  ; chats : Chats.t
  ; chat_ids : int64 list
  ; chat_actions : Chat_actions.t
  ; unread_message_ids : Unread_messages.t
  ; updated_message_ids : Message.Id.Set.t
  ; mutations : (Request.t -> t -> [ `Return of t | `Apply of t | `Skip ]) list
  }

let empty =
  { is_ready = Async.Ivar.create ()
  ; users = Users.empty
  ; users_status = Users_status.empty
  ; chats = Chats.empty
  ; chat_ids = []
  ; chat_actions = Chat_actions.empty
  ; unread_message_ids = Unread_messages.empty
  ; updated_message_ids = Message.Id.Set.empty
  ; mutations = []
  }
;;

let create () =
  let open Async.Mvar in
  let var = create () in
  set var empty;
  var
;;

(*Mutations*)
let add_mutation state f = { state with mutations = state.mutations @ [ f ] }

let run_mutations state request =
  let mutations = state.mutations in
  let state = { state with mutations = [] } in
  List.fold mutations ~init:state ~f:(fun state mutation ->
      match mutation request state with
      | `Return state -> { state with mutations = state.mutations @ [ mutation ] }
      | `Apply state -> state
      | `Skip -> { state with mutations = state.mutations @ [ mutation ] })
;;

(*Updated messages*)
let append_updated_message_id state id =
  { state with updated_message_ids = Message.Id.Set.add state.updated_message_ids id }
;;

let remove_updated_message_id state id =
  { state with updated_message_ids = Message.Id.Set.remove state.updated_message_ids id }
;;

let is_message_id_updated { updated_message_ids; _ } =
  Message.Id.Set.mem updated_message_ids
;;

(*Updated messages*)
let append_unread_message_id state chat_id message_id =
  { state with
    unread_message_ids =
      Unread_messages.update
        state.unread_message_ids
        chat_id
        ~f:
          (Option.value_map ~default:[ message_id ] ~f:(fun messages ->
               message_id :: messages))
  }
;;

let remove_unread_message_ids state chat_id =
  { state with
    unread_message_ids = Unread_messages.remove state.unread_message_ids chat_id
  }
;;

let unread_message_ids { unread_message_ids; _ } chat_id =
  chat_id |> Unread_messages.find unread_message_ids |> Option.value ~default:[]
;;

(*Events*)
let when_ready { is_ready; _ } = Async.Ivar.read is_ready

(*Users*)
let set_user state user =
  { state with users = Users.set state.users ~key:(User.id user) ~data:user }
;;

let user { users; _ } = Users.find users

let users_by_name state name =
  Users.to_alist state.users
  |> List.map ~f:snd
  |> List.filter ~f:String.Caseless.(fun u -> User.full_name u = name)
;;

let lookup_users state q =
  Users.to_alist state.users
  |> List.map ~f:snd
  |> List.filter ~f:(fun user ->
         let full_name = User.full_name user in
         String.Caseless.is_prefix full_name ~prefix:q)
;;

(*Users status*)
let update_user_status state user new_status =
  let key = User.id user in
  match Users_status.find state.users_status key with
  | Some status when status = new_status -> None
  | Some _ | None ->
    let users_status = Users_status.set state.users_status ~key ~data:new_status in
    Some { state with users_status }
;;

(*Chats*)
let set_chat_ids state chat_ids = { state with chat_ids }

let set_chat state chat =
  { state with chats = Chats.set state.chats ~key:(Chat.id chat) ~data:chat }
;;

let chat state id = Chats.find state.chats id

let lookup_chats state q =
  Chats.to_alist state.chats
  |> List.map ~f:snd
  |> List.filter ~f:(fun chat -> String.Caseless.is_prefix (Chat.title chat) ~prefix:q)
;;

let find_chat state q =
  let user_ids = q |> users_by_name state |> List.map ~f:User.id |> User.Id.Set.of_list in
  let matches_user chat =
    match Chat.typ chat with
    | Private { user_id; _ } | Secret { user_id; _ } -> User.Id.Set.mem user_ids user_id
    | _ -> false
  in
  let matches_title chat = String.Caseless.equal (Chat.title chat) q in
  state.chats
  |> Chats.to_alist
  |> List.map ~f:snd
  |> List.find ~f:(fun chat -> matches_user chat || matches_title chat)
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
