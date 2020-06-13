(*
     ArrayParser.rei

     Specialized parser for arrays of string
 *)

type t

module Baseline : sig
  (*
    [Baseline.t] contains a baseline parse result, to be used
    to create an incremental delta.
  *)
  type t
end

module Delta : sig
  (* [Delta.t] describes updates to a Tree for incremental parsing *)
  type t

  (*
     [create(baseline, oldStartLine, oldEndLine, updates)] creates an
     incremental update to speed up the [parse].
   *)
  val create : Baseline.t -> int -> int -> string array -> t
end

(*
   [parse(parser, delta, contents] parses a document, provided by a string
   array [contents], and returns a tuple of a `Tree.t` and `Baseline.t.`

   The `baseline` can be used to create a delta, and optionally provided for
   incremental parsing.
 *)
val parse : Parser.t -> Delta.t option -> string array -> Tree.t * Baseline.t
