open Async
open Core
open Tdlib.Models

module Completion = struct
  open Readline

  let state : State.t Mvar.Read_write.t option ref = ref None
  let index : int ref = ref 0

  let prepare_completion s =
    let result =
      if Option.is_none (Completion_quote_character.get ())
      then String.substr_replace_all s ~pattern:" " ~with_:"\\ "
      else s
    in
    Completion_entry_function.completion_result_of_string result
  ;;

  let completion_entry text completion_state =
    let state = Mvar.peek_exn (Option.value_exn !state) in
    let users = State.lookup_users state text in
    let chats = State.lookup_chats state text in
    let results = List.map users ~f:User.full_name @ List.map chats ~f:Chat.title in
    if completion_state = 0 then index := 0;
    let result = List.nth results !index in
    index := !index + 1;
    match result with
    | Some result -> Some (prepare_completion result)
    | None ->
      Option.map
        ~f:Completion_entry_function.completion_result_of_char_ptr
        (filename_completion_function text completion_state)
  ;;

  let attempted_completion text start _end =
    if start = 0 then Some (completion_matches text completion_entry) else None
  ;;

  let rec is_char_quoted text index =
    match index with
    | 0 -> false
    | i -> Char.equal text.[i - 1] '\\' && not (is_char_quoted text (index - 1))
  ;;
end

let configure state' =
  Completion.state := Some state';
  Readline.Attempted_completion_function.set Completion.attempted_completion;
  Readline.Completion_entry_function.set Completion.completion_entry;
  Readline.Char_is_quoted.set Completion.is_char_quoted;
  Readline.Completer_quote_characters.set (Some "\"");
  Readline.Completer_word_break_characters.set (Some " \t");
  Readline.Filename_quote_characters.set (Some " ")
;;

let parse exec =
  let open Async in
  let read () = Readline.readline_opt "・" in
  let%map cmd = In_thread.run read in
  match cmd with
  | Some cmd ->
    Readline.add_history cmd;
    exec cmd
  | None -> ()
;;

module Color = struct
  let colors =
    [ 222, 57, 183
    ; 82, 199, 43
    ; 199, 65, 226
    ; 79, 197, 84
    ; 108, 91, 241
    ; 165, 190, 48
    ; 156, 82, 223
    ; 134, 194, 81
    ; 36, 90, 218
    ; 213, 176, 63
    ; 69, 119, 249
    ; 94, 153, 47
    ; 188, 93, 211
    ; 57, 161, 78
    ; 125, 111, 232
    ; 143, 145, 39
    ; 77, 88, 201
    ; 64, 200, 135
    ; 186, 74, 160
    ; 55, 122, 43
    ; 35, 95, 201
    ; 222, 94, 41
    ; 84, 126, 236
    ; 201, 125, 44
    ; 85, 145, 236
    ; 214, 74, 139
    ; 80, 115, 204
    ; 227, 124, 212
    ; 73, 94, 183
    ; 176, 134, 234
    ; 112, 99, 198
    ; 144, 91, 184
    ]
  ;;

  let gen_color key = List.nth_exn colors (key mod List.length colors)
end

module Style = struct
  type attribute =
    | Foreground of (int * int * int)
    | Background of (int * int * int)
    | Normal
    | Bold
    | Dim
    | Underlined
    | Reversed

  let value_of_attribute = function
    | Foreground (r, g, b) -> sprintf "38;2;%d;%d;%d" r g b
    | Background (r, g, b) -> sprintf "48;2;%d;%d;%d" r g b
    | Normal -> "0"
    | Bold -> "1"
    | Dim -> "2"
    | Underlined -> "4"
    | Reversed -> "7"
  ;;

  let set_custom attributes =
    let formats = List.map attributes ~f:value_of_attribute in
    "\027[" ^ String.concat formats ~sep:";" ^ "m"
  ;;

  let unset_custom = set_custom [ Normal ]
  let with_format attributes str = set_custom attributes ^ str ^ unset_custom
  let secondary = [ Dim; Foreground (192, 192, 192) ]
  let tertiary = [ Dim; Foreground (128, 128, 128) ]
  let error = [ Foreground (214, 53, 64) ]
  let success = [ Foreground (91, 158, 55) ]
  let prominent = [ Bold; Foreground (255, 255, 255) ]
  let to_string_prominent = with_format prominent
  let to_string_success = with_format success
  let to_string_error = with_format error
end

let printf style fmt =
  Printf.ksprintf
    (fun s ->
      let open Readline in
      let point = Point.get () in
      let text = copy_text 0 (End.get ()) in
      save_prompt ();
      ignore (replace_line "" 0);
      ignore (redisplay ());
      print_string (Style.set_custom style ^ s ^ Style.unset_custom);
      Out_channel.flush stdout;
      restore_prompt ();
      ignore (replace_line text 0);
      Point.set point;
      ignore (redisplay ()))
    fmt
;;

let rprintf style fmt =
  Printf.ksprintf
    (fun s ->
      print_string (Style.set_custom style ^ s ^ Style.unset_custom);
      Out_channel.flush stdout)
    fmt
;;

let sprintf style fmt =
  Printf.ksprintf (fun s -> Style.set_custom style ^ s ^ Style.unset_custom) fmt
;;
