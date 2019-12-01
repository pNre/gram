module Models = struct
  include Models
end

module Client = struct
  open Core
  open Tdjson
  open Models.Request

  type t = Tdjson.client

  let init = td_json_client_create
  let deinit = td_json_client_destroy

  let request_of_string s =
    Result.try_with (fun _ -> t_of_yojson (Yojson.Safe.from_string s))
  ;;

  let execute ?(client = None) req =
    let serialized = req |> yojson_of_t |> Yojson.Safe.to_string in
    let response = td_json_client_execute client serialized in
    Option.map ~f:request_of_string response
  ;;

  let send client req =
    let json = yojson_of_t req in
    let serialized = Yojson.Safe.to_string json in
    td_json_client_send client serialized
  ;;

  let receive ?(timeout = 10.0) client =
    Async.In_thread.run (fun _ ->
        let data = td_json_client_receive timeout client in
        Option.map ~f:request_of_string data)
  ;;
end
