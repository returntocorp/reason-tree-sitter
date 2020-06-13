(*
    Helpers to connect TSNode with a syntax highlighting solution
 *)

open EditorCoreTypes

let getErrorRanges (node : Node.t) =
  let rec f (node : Node.t) (errors : Range.t list) =
    let hasError = Node.hasError node in
    match hasError with
    | false -> errors
    | true -> (
        let isError = Node.isError node in
        match isError with
        | true -> Node.getRange node :: errors
        | false ->
            let children = Node.getChildren node in
            let i = ref 0 in
            List.fold_left
              (fun prev curr ->
                incr i;
                f curr prev)
              errors children )
  in
  f node []

type scope = int * string

module Token = struct
  type t = Location.t * Location.t * scope list * string

  let ofNode ~getTokenName scopes (node : Node.t) =
    let isNamed = Node.isNamed node in
    let start = Node.getStartPoint node in
    let stop = Node.getEndPoint node in
    let range = Range.create ~start ~stop in
    let tokenName = getTokenName range in
    match isNamed with
    | false -> (start, stop, scopes, tokenName)
    | true ->
        let nodeType = Node.getType node in
        let scopes = (0, nodeType) :: scopes in
        (start, stop, scopes, tokenName)

  let getPosition ((pos, _, _, _) : t) = pos

  let getEndPosition ((_, endPos, _, _) : t) = endPos

  let getName ((_, _, _, name) : t) = name

  let _showScope (idx, s) = Printf.sprintf "(%n:%s)" idx s

  let show ((p, e, scopes, tok) : t) =
    Printf.sprintf "Token(%s - %s:%s|%s)" (Location.toString p)
      (Location.toString e)
      (scopes |> List.map _showScope |> String.concat ".")
      tok
end

let getParentScopes (node : Node.t) =
  let rec f (node : Node.t) (scopes : scope list) =
    let parent = Node.getParent node in
    if Node.isNull parent then scopes
    else
      match Node.isNamed parent with
      | true ->
          f parent
            ((Node.getBoundedNamedIndex parent, Node.getType parent) :: scopes)
      | false -> f parent scopes
  in
  f node []

let createArrayTokenNameResolver (v : string array) (range : Range.t) =
  if range.start.line <> range.stop.line then ""
  else if range.start.line >= Index.fromZeroBased (Array.length v) then ""
  else
    let lineNumber = Index.toZeroBased range.start.line in
    let line = v.(lineNumber) in
    let len = String.length line in
    if len = 0 || range.start.column = range.stop.column then ""
    else
      Printf.sprintf {|"%s"|}
        (String.sub line
           (Index.toZeroBased range.start.column)
           ( Index.toZeroBased range.stop.column
           - Index.toZeroBased range.start.column ))

let getTokens ~getTokenName ~(range : Range.t) (node : Node.t) =
  let nodeToUse =
    Node.getDescendantForPointRange node
      (Index.toZeroBased range.start.line)
      (Index.toZeroBased range.start.column)
      (Index.toZeroBased range.stop.line)
      (Index.toZeroBased range.stop.column)
  in
  let parentScopes = getParentScopes node |> List.rev in
  let rec f index (n : Node.t) (tokens : Token.t list) (scopes : scope list) =
    let start = Node.getStartPoint n in
    let stop = Node.getEndPoint n in
    if
      stop.line < range.start.line
      || (stop.line = range.start.line && stop.column < range.start.column)
      || start.line > range.stop.line
      || (start.line = range.stop.line && stop.column > range.stop.column)
    then tokens
    else
      let childCount = Node.getChildCount n in
      match childCount with
      | 0 -> Token.ofNode ~getTokenName scopes n :: tokens
      | _ ->
          let children = Node.getChildren n in
          let newScopes =
            match Node.isNamed n with
            | false -> scopes
            | true -> (index, Node.getType n) :: scopes
          in
          let _, tokens =
            List.fold_left
              (fun prev curr ->
                let index, tokens = prev in
                let idx =
                  match Node.isNamed curr with
                  | true -> Node.getBoundedNamedIndex curr
                  | false -> index
                in
                let newTokens = f idx curr tokens newScopes in
                (idx, newTokens))
              (0, tokens) children
          in
          tokens
  in
  f 0 nodeToUse [] parentScopes |> List.rev
