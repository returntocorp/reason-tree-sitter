(*
     Stubs for bindings to the `TSTree` object
 *)

open EditorCoreTypes
open NativeTypes

type t = Tree.t * node

let getTree (v : t) =
  let _, tree = v in
  tree

external _toString : node -> string = "rets_node_string"

external _getChildCount : node -> int = "rets_node_child_count"

external _getChild : node -> int -> node = "rets_node_child"

external _getParent : node -> node = "rets_node_parent"

external _getNamedChildCount : node -> int = "rets_node_named_child_count"

external _getNamedChild : node -> int -> node = "rets_node_named_child"

external _getBoundedNamedIndex : node -> int = "rets_node_bounded_named_index"

external _getNamedIndex : node -> int = "rets_node_named_index"

external _getIndex : node -> int = "rets_node_index"

external _getDescendantForPointRange : node -> int -> int -> int -> int -> node
  = "rets_node_descendant_for_point_range"

external _getStartByte : node -> int = "rets_node_start_byte"

external _getEndByte : node -> int = "rets_node_end_byte"

external _getStartPoint : node -> Location.t = "rets_node_start_point"

external _getEndPoint : node -> Location.t = "rets_node_end_point"

external _hasChanges : node -> bool = "rets_node_has_changes"

external _hasError : node -> bool = "rets_node_has_error"

external _isMissing : node -> bool = "rets_node_is_missing"

external _isNull : node -> bool = "rets_node_is_null"

external _isNamed : node -> bool = "rets_node_is_named"

external _isError : node -> bool = "rets_node_is_error"

external _isExtra : node -> bool = "rets_node_is_extra"

external _getSymbol : node -> int = "rets_node_symbol"

external _getType : node -> string = "rets_node_type"

let wrap0 f v =
  let _, node = v in
  f node

let toString = (wrap0 _toString : t -> string)

let getChildCount = (wrap0 _getChildCount : t -> int)

let getChild (v : t) idx =
  let tree, node = v in
  (tree, _getChild node idx)

let getParent =
  ( fun (v : t) ->
      let tree, node = v in
      (tree, _getParent node)
    : t -> t )

let getNamedChildCount = (wrap0 _getNamedChildCount : t -> int)

let getNamedChild (v : t) idx =
  let tree, node = v in
  (tree, _getNamedChild node idx)

let getBoundedNamedIndex (v : t) =
  let _, node = v in
  _getBoundedNamedIndex node

let getNamedIndex (v : t) =
  let _, node = v in
  _getNamedIndex node

let getIndex (v : t) =
  let _, node = v in
  _getIndex node

let getDescendantForPointRange (v : t) r0 c0 r1 c1 =
  let tree, node = v in
  (tree, _getDescendantForPointRange node r0 c0 r1 c1)

let getStartByte = (wrap0 _getStartByte : t -> int)

let getEndByte = (wrap0 _getEndByte : t -> int)

let getStartPoint = (wrap0 _getStartPoint : t -> Location.t)

let getEndPoint = (wrap0 _getEndPoint : t -> Location.t)

let hasChanges = (wrap0 _hasChanges : t -> bool)

let hasError = (wrap0 _hasError : t -> bool)

let isMissing = (wrap0 _isMissing : t -> bool)

let isNull = (wrap0 _isNull : t -> bool)

let isNamed = (wrap0 _isNamed : t -> bool)

let isError = (wrap0 _isError : t -> bool)

let isExtra = (wrap0 _isExtra : t -> bool)

let getSymbol = (wrap0 _getSymbol : t -> int)

let getType = (wrap0 _getType : t -> string)

let getChildren (node : t) =
  let i = ref 0 in
  let count = getChildCount node in
  let children = ref [] in
  while !i < count do
    let child = getChild node !i in
    children := child :: !children;
    incr i
  done;
  List.rev !children

let getRange (node : t) =
  let start = getStartPoint node in
  let stop = getEndPoint node in
  Range.create ~start ~stop
