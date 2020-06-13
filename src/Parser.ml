(*
     Stubs for bindings to the `TSParser` object
 *)

type t

(* Parsers for particular syntaxes *)
external json : unit -> t = "rets_parser_new_json"
external c : unit -> t = "rets_parser_new_c"

(* General parser methods *)
external parseString : t -> string -> Tree.t = "rets_parser_parse_string"

type readFunction = int -> int -> int -> string option

external _parse : t -> Tree.t option -> readFunction -> Tree.t
  = "rets_parser_parse"

let _parse_read_fn = (ref (fun _ _ _ -> None) : readFunction ref)

let _parse_read (byteOffset : int) (line : int) (col : int) =
  !_parse_read_fn byteOffset line col

let parse (parser : t) (tree : Tree.t option) readFunction =
  _parse_read_fn := readFunction;
  _parse parser tree _parse_read

let () = Callback.register "rets__parse_read" _parse_read
