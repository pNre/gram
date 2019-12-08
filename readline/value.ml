open Ctypes

module type Named = sig
  val name : string
end

module type Base = sig
  include Named

  type t

  val typ : t Ctypes.typ
end

module R (S : Base) = struct
  let value = Foreign.foreign_value S.name S.typ
  let get () = !@value
end

module RW (S : Base) = struct
  include R (S)

  let set v = value <-@ v
end

module RW_string (S : Named) = struct
  module Wrapped = RW (struct
    include S

    type t = char ptr option

    let typ = ptr_opt char
  end)

  let get = Wrapped.get

  let set s =
    let current = get () in
    Option.iter (fun p -> Types.free (to_voidp p)) current;
    Wrapped.set (Option.map Types.char_ptr_of_string s)
  ;;
end

