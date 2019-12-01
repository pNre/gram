open Ctypes
open Foreign

let strndup = foreign "strndup" (string @-> size_t @-> returning (ptr char))
let strlen = foreign "strlen" (ptr char @-> returning int)
let free = foreign "free" (ptr void @-> returning void)

let char_ptr_to_string p =
  let len = strlen p in
  let chars = CArray.from_ptr p len in
  let bytes = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set bytes i (CArray.unsafe_get chars i)
  done;
  Bytes.unsafe_to_string bytes
;;

let char_ptr_to_string_and_free p =
  let s = char_ptr_to_string p in
  free (to_voidp p);
  s
;;

let char_ptr_of_string s =
  let length = String.length s in
  strndup s (Unsigned.Size_t.of_int length)
;;
