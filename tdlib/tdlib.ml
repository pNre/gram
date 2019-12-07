module Models = struct
  include Models
end

module Client = struct
  open Core
  open Tdjson
  open Models.Request

  type t =
    { client : Tdjson.client
    ; timeout : float
    ; debug : [ `Sent | `Received ] -> string -> unit
    }

  let init ?(debug = fun _ _ -> ()) () =
    { client = td_json_client_create (); timeout = 10.; debug }
  ;;

  let deinit { client; _ } = td_json_client_destroy client

  let request_of_string s =
    Result.try_with (fun _ -> t_of_yojson (Yojson.Safe.from_string s))
  ;;

  let execute ?(client = None) req =
    let serialized = req |> yojson_of_t |> Yojson.Safe.to_string in
    let response =
      td_json_client_execute
        (Option.map client ~f:(fun { client; _ } -> client))
        serialized
    in
    Option.map ~f:request_of_string response
  ;;

  let send' { client; debug; _ } req =
    let uuid = Uuid.to_string (Uuid_unix.create ()) in
    let extra = `Assoc [ "@extra", `String uuid ] in
    let req = yojson_of_t req in
    let json = Yojson.Safe.Util.combine extra req in
    let serialized = Yojson.Safe.to_string json in
    debug `Sent serialized;
    td_json_client_send client serialized;
    uuid
  ;;

  let send client req = send' client req |> ignore

  let receive { client; timeout; debug; _ } =
    Async.In_thread.run (fun _ ->
        let data = td_json_client_receive timeout client in
        Option.iter data ~f:(debug `Received);
        Option.map data ~f:request_of_string)
  ;;
end
