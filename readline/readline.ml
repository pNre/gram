open Ctypes
open Foreign

(*Readline*)
let readline_ =
  foreign ~release_runtime_lock:true "readline" (string @-> returning (ptr_opt char))
;;

let readline_opt prompt =
  prompt |> readline_ |> Option.map Types.char_ptr_to_string_and_free
;;

let rec readline prompt =
  match readline_opt prompt with
  | None -> readline prompt
  | Some s -> s
;;

let add_history = foreign "add_history" (string @-> returning void)

(*Redisplay*)
let redisplay = foreign "rl_redisplay" (void @-> returning int)
let replace_line = foreign "rl_replace_line" (string @-> int @-> returning void)
let clear_visible_line = foreign "rl_clear_visible_line" (void @-> returning void)
let save_prompt = foreign "rl_save_prompt" (void @-> returning void)
let restore_prompt = foreign "rl_restore_prompt" (void @-> returning void)

(*Modifying text*)
let insert_text = foreign "rl_insert_text" (string @-> returning int)
let copy_text_ = foreign "rl_copy_text" (int @-> int @-> returning (ptr char))

let copy_text start end_ =
  let text = copy_text_ start end_ in
  Types.char_ptr_to_string_and_free text
;;

(*Keys*)
let insert = foreign "rl_insert" (int @-> int @-> returning int)

let bind_key =
  foreign "rl_bind_key" (char @-> funptr (int @-> int @-> returning int) @-> returning int)
;;

(*Vars*)
module End = Value.R (struct
  type t = int

  let name = "rl_end"
  let typ = int
end)

module Point = Value.RW (struct
  type t = int

  let name = "rl_point"
  let typ = int
end)

(*Autocomplete*)

module Completion_entry_function = struct
  type completion_result = char ptr

  let completion_result : completion_result typ = ptr char
  let completion_result_opt : completion_result option typ = ptr_opt char
  let completion_result_of_string s : completion_result = Types.char_ptr_of_string s
  let completion_result_of_char_ptr (p : char ptr) : completion_result = p

  include (val dynamic_funptr
                 ~runtime_lock:true
                 (string @-> int @-> returning completion_result_opt))

  let value = Foreign.foreign_value "rl_completion_entry_function" t_opt

  let set v =
    Option.iter free !@value;
    value <-@ Option.map of_fun v
  ;;
end

module Attempted_completion_function = struct
  type attempted_completion_result = char ptr ptr

  let attempted_completion_result_opt : attempted_completion_result option typ =
    ptr_opt (ptr char)
  ;;

  include (val dynamic_funptr
                 ~runtime_lock:true
                 (string @-> int @-> int @-> returning attempted_completion_result_opt))

  let value = Foreign.foreign_value "rl_attempted_completion_function" t_opt

  let set v =
    Option.iter free !@value;
    value <-@ Option.map of_fun v
  ;;
end

module Attempted_completion_over = Value.RW (struct
  type t = bool

  let name = "rl_attempted_completion_over"
  let typ = bool
end)

module Char_is_quoted = Value.RW (struct
  type t = string -> int -> bool

  let name = "rl_char_is_quoted_p"
  let typ = funptr ~runtime_lock:true (string @-> int @-> returning bool)
end)

module Completer_word_break_characters = Value.RW_string (struct
  let name = "rl_completer_word_break_characters"
end)

module Filename_quote_characters = Value.RW_string (struct
  let name = "rl_filename_quote_characters"
end)

module Completer_quote_characters = Value.RW_string (struct
  let name = "rl_completer_quote_characters"
end)

module Completion_quote_character = struct
  module Wrapped = Value.R (struct
    type t = char

    let name = "rl_completion_quote_character"
    let typ = char
  end)

  let get () =
    match Wrapped.get () with
    | '\x00' -> None
    | c -> Some c
  ;;
end

let filename_completion_function =
  foreign "rl_filename_completion_function" (string @-> int @-> returning (ptr_opt char))
;;

let completion_matches =
  let completion_entry_function_typ =
    funptr (string @-> int @-> returning Completion_entry_function.completion_result_opt)
  in
  foreign
    "rl_completion_matches"
    (string @-> completion_entry_function_typ @-> returning (ptr (ptr char)))
;;

(*Misc*)
let () =
  Foreign.report_leaked_funptr := ignore;
  foreign_value "rl_readline_name" string <-@ "tgcaml";
  foreign_value "rl_catch_signals" int <-@ 0;
  foreign_value "rl_complete_with_tilde_expansion" int <-@ 1
;;
