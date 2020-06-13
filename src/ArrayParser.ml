(*
     ArrayParser.re

     Specialized parser for arrays of string
 *)

type t

module Baseline = struct
  type t = { lengths : int array; tree : Tree.t }

  let create ~lengths ~tree () = { lengths; tree }
end

module Delta = struct
  type t = {
    tree : Tree.t;
    startLine : int;
    oldEndLine : int;
    oldOffsets : int array;
    newLines : string array;
  }

  let getOffsetToLine startLine endLine (lines : int array) =
    let i = ref startLine in
    let offset = ref 0 in
    while !i < endLine do
      offset := !offset + lines.(!i);
      incr i
    done;
    !offset

  let getOffsetToLineStr startLine endLine (lines : string array) =
    let i = ref startLine in
    let offset = ref 0 in
    while !i < endLine do
      offset := !offset + String.length lines.(!i) + 1;
      incr i
    done;
    !offset

  let create (baseline : Baseline.t) (startLine : int) (oldEndLine : int)
      (newLines : string array) =
    let ({ lengths; tree } : Baseline.t) = baseline in
    let startByte = getOffsetToLine 0 startLine lengths in
    let oldEndByte = getOffsetToLine 0 oldEndLine lengths in
    let len = Array.length newLines in
    let newEndByte = startByte + getOffsetToLineStr 0 len newLines in
    let newEndLine = startLine + len in
    let newTree =
      Tree.edit tree startByte oldEndByte newEndByte startLine oldEndLine
        newEndLine
    in
    {
      tree = newTree;
      startLine;
      oldEndLine;
      newLines;
      oldOffsets = baseline.lengths;
    }
end

let parse (parser : Parser.t) (delta : Delta.t option) (lines : string array) =
  let len = Array.length lines in
  let byteOffsets = (Array.make len 0 : int array) in
  (* The interop between C <-> OCaml is expensive for large files.
     We should look to see if we can instead access the array directly
     from the C side. *)
  let f _byteOffset line col =
    if line < len then
      let v = lines.(line) ^ "\n" in
      let strlen = String.length v in
      if col < strlen then
        let ret = String.sub v col (strlen - col) in
        (Some ret [@explicit_arity])
      else None
    else None
  in
  let i = ref 0 in
  while !i < len do
    let idx = !i in
    byteOffsets.(idx) <- String.length lines.(idx) + 1;
    incr i
  done;
  let oldTree =
    match delta with
    | ((Some { tree; _ })[@explicit_arity]) -> ( Some tree [@explicit_arity] )
    | None -> None
  in
  let tree = Parser.parse parser oldTree f in
  let baseline = Baseline.create ~tree ~lengths:byteOffsets () in
  (tree, baseline)
