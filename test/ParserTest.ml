open TestFramework
open Treesitter

let () =
  describe "Parser" (fun { describe; _ } ->
    describe "allocation" (fun { test; _ } ->
        test "finalizer gets called for parser" (fun { expect; _ } ->
            let jsonParser = Parser.json () in
            let callCount = ref 0 in
            Gc.finalise_last (fun () -> incr callCount) jsonParser;
            Gc.full_major ();
            (expect.int !callCount).toBe 1);
        test "finalizer gets called for tree" (fun { expect; _ } ->
            let jsonParser = Parser.json () in
            let tree = Parser.parseString jsonParser "[1, \"a\", null]" in
            let callCount = ref 0 in
            Gc.finalise_last (fun () -> incr callCount) tree;
            Gc.full_major ();
            (expect.int !callCount).toBe 1));
    describe "json" (fun { test; _ } ->
        test "simple parsing case" (fun { expect; _ } ->
            let jsonParser = Parser.json () in
            let tree = Parser.parseString jsonParser "[1, \"2\"]" in
            let node = Tree.getRootNode tree in
            let ret = Node.toString node in
            prerr_endline ("RET: " ^ ret);
            (expect.string ret).toEqual
              "(value (array (number) (string (string_content))))";
            (expect.int (Node.getChildCount node)).toBe 1;
            (expect.string (Node.getType node)).toEqual "value";
            let arrayNode = Node.getChild node 0 in
            (expect.string (Node.getType arrayNode)).toEqual "array";
            (expect.int (Node.getNamedChildCount arrayNode)).toBe 2;
            (expect.int (Node.getChildCount arrayNode)).toBe 5;
            let array0 = Node.getNamedChild arrayNode 0 in
            let array1 = Node.getNamedChild arrayNode 1 in
            (expect.string (Node.getType array0)).toEqual "number";
            (expect.string (Node.getType array1)).toEqual "string"));
    describe "c" (fun { test; _ } ->
        test "basic parse case" (fun { expect; _ } ->
            let jsonParser = Parser.c () in
            let tree =
              Parser.parseString jsonParser "int main() { return 1; }"
            in
            let node = Tree.getRootNode tree in
            let ret = Node.toString node in
            prerr_endline ("RET: " ^ ret);
            (expect.string ret).toEqual
              "(translation_unit (function_definition type: (primitive_type) \
               declarator: (function_declarator declarator: (identifier) \
               parameters: (parameter_list)) body: (compound_statement \
               (return_statement (number_literal)))))")))
