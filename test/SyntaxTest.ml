open EditorCoreTypes
open Treesitter
open TestFramework

let () =
  describe "Syntax" (fun { describe; _ } ->
    let jsonParser = Parser.json () in
    let simpleArray = [| "[1, \"200\"]" |] in
    let tree, _ = ArrayParser.parse jsonParser None simpleArray in
    let simpleNode = Tree.getRootNode tree in
    let simpleNameResolver = Syntax.createArrayTokenNameResolver simpleArray in
    let errorArray = [| "[1,  ]" |] in
    let tree, _ = ArrayParser.parse jsonParser None errorArray in
    let errorNode = Tree.getRootNode tree in
    let errorNameResolver = Syntax.createArrayTokenNameResolver errorArray in
    let objectArray = [| "{ \"key\": \"value\" " |] in
    let tree, _ = ArrayParser.parse jsonParser None objectArray in
    let objectNode = Tree.getRootNode tree in
    let objectNameResolver = Syntax.createArrayTokenNameResolver objectArray in
    let range =
      Range.create
        ~start:(Location.create ~line:Index.zero ~column:Index.zero)
        ~stop:
          (Location.create
             ~line:
               (let open Index in
               zero + 1)
             ~column:Index.zero)
    in
    describe "getErrorRanges" (fun { test; _ } ->
        test "returns empty list when none" (fun { expect; _ } ->
            let errors = Syntax.getErrorRanges simpleNode in
            (expect.int (List.length errors)).toBe 0);
        test "returns error when present" (fun { expect; _ } ->
            let errors = Syntax.getErrorRanges errorNode in
            (expect.int (List.length errors)).toBe 1;
            let errorRange = List.hd errors in
            (expect.int (errorRange.start.line :> int)).toBe 0;
            (expect.int (errorRange.stop.line :> int)).toBe 0;
            (expect.int (errorRange.start.column :> int)).toBe 2;
            (expect.int (errorRange.stop.column :> int)).toBe 3));
    describe "getTokens" (fun { test; _ } ->
        test "returns list of tokens for object in  success case"
          (fun { expect; _ } ->
            prerr_endline "--OBJECT--";
            let tokens =
              Syntax.getTokens ~getTokenName:objectNameResolver ~range
                objectNode
            in
            List.iter (fun v -> prerr_endline (Syntax.Token.show v)) tokens;
            (expect.int (List.length tokens)).toBe 9);
        test "returns list of tokens in success case" (fun { expect; _ } ->
            prerr_endline "--ARRAY--";
            let tokens =
              Syntax.getTokens ~getTokenName:simpleNameResolver ~range
                simpleNode
            in
            List.iter (fun v -> prerr_endline (Syntax.Token.show v)) tokens;
            (expect.int (List.length tokens)).toBe 7);
        test "returns list of tokens in error case" (fun { expect; _ } ->
            prerr_endline "--ERROR--";
            let tokens =
              Syntax.getTokens ~getTokenName:errorNameResolver ~range errorNode
            in
            List.iter (fun v -> prerr_endline (Syntax.Token.show v)) tokens;
            (expect.int (List.length tokens)).toBe 4));
    describe "getParentScopes" (fun { test; _ } ->
        test "returns empty list for root" (fun { expect; _ } ->
            let scopes = Syntax.getParentScopes simpleNode in
            (expect.int (List.length scopes)).toBe 0);
        test "returns single item for first child" (fun { expect; _ } ->
            let firstChild = Node.getChild simpleNode 0 in
            let scopes = Syntax.getParentScopes firstChild in
            (expect.bool (scopes = [ (0, "value") ])).toBe true);
        test "returns multiple item for second child" (fun { expect; _ } ->
            let firstChild = Node.getChild simpleNode 0 in
            let secondChild = Node.getChild firstChild 0 in
            let scopes = Syntax.getParentScopes secondChild in
            (expect.bool (scopes = [ (0, "value"); (0, "array") ])).toBe true)))
