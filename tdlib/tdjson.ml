open Ctypes
open Foreign

type client = unit ptr

let client : client typ = ptr void
let client_opt : client option typ = ptr_opt void
let td_json_client_create = foreign "td_json_client_create" (void @-> returning client)

let td_json_client_send =
  foreign "td_json_client_send" (client @-> string @-> returning void)
;;

let td_json_client_receive =
  Fun.flip
    (foreign
       ~release_runtime_lock:true
       "td_json_client_receive"
       (client @-> double @-> returning string_opt))
;;

let td_json_client_execute =
  foreign
    "td_json_client_execute"
    ~release_runtime_lock:true
    (client_opt @-> string @-> returning string_opt)
;;

let td_json_client_destroy = foreign "td_json_client_destroy" (client @-> returning void)
