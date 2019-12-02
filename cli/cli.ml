open Async
open Core
open Tdlib.Models

module Completion = struct
  let state = ref (Mvar.create ())
  let index = ref 0

  let prepare_completion s =
    let result =
      if Option.is_none (Readline.Completion_quote_character.get ())
      then String.substr_replace_all s ~pattern:" " ~with_:"\\ "
      else s
    in
    Readline.Completion_entry_function.completion_result_of_string result
  ;;

  let completion_entry text completion_state =
    let state = Mvar.peek_exn !state in
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
        ~f:Readline.Completion_entry_function.completion_result_of_char_ptr
        (Readline.filename_completion_function text completion_state)
  ;;

  let attempted_completion text start _end =
    if start = 0 then Some (Readline.completion_matches text completion_entry) else None
  ;;

  let rec is_char_quoted text index =
    match index with
    | 0 -> false
    | i -> text.[i - 1] = '\\' && not (is_char_quoted text (index - 1))
  ;;
end

let configure state' =
  Completion.state := state';
  Readline.Attempted_completion_function.set Completion.attempted_completion;
  Readline.Completion_entry_function.set Completion.completion_entry;
  Readline.Char_is_quoted.set Completion.is_char_quoted;
  Readline.Completer_quote_characters.set (Some "\"");
  Readline.Completer_word_break_characters.set (Some " \t");
  Readline.Filename_quote_characters.set (Some " ")
;;

let parse client state exec () =
  let open Async in
  let read () = Readline.readline_opt "$ " in
  let%bind cmd = In_thread.run read in
  match cmd with
  | Some cmd ->
    Readline.add_history cmd;
    let state = Mvar.peek_exn state in
    exec client state cmd
  | None -> return ()
;;

let color_of_key k =
  let colors = [ `Red; `Green; `Yellow; `Magenta; `Cyan ] in
  let index = k mod List.length colors in
  List.nth_exn colors index
;;

let print ?(style = []) ?(key = None) s =
  let open Readline in
  let point = Point.get () in
  let text = copy_text 0 (End.get ()) in
  save_prompt ();
  ignore (replace_line "" 0);
  ignore (redisplay ());
  let style =
    Option.value_map ~default:style ~f:(fun key -> color_of_key key :: style) key
  in
  Console.Ansi.printf style "%s" s;
  restore_prompt ();
  ignore (replace_line text 0);
  Point.set point;
  ignore (redisplay ())
;;
