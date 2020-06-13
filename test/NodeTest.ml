open TestFramework
open Treesitter

let () =
  describe "Node" (fun { describe; _ } ->
    let jsonParser = Parser.json () in
    let tree, _ = ArrayParser.parse jsonParser None [| "[1,"; "\"2\""; "]" |] in
    let simpleNode = Tree.getRootNode tree in
    let tree, _ = ArrayParser.parse jsonParser None [| "[,]" |] in
    let errorNode = Tree.getRootNode tree in
    describe "getIndex / getNamedIndex" (fun { test; _ } ->
        test "getNamedIndex returns correct values" (fun { expect; _ } ->
            let firstChild = Node.getNamedChild simpleNode 0 in
            let child1 = Node.getNamedChild firstChild 0 in
            let child2 = Node.getNamedChild firstChild 1 in
            (expect.int (Node.getNamedIndex child1)).toBe 0;
            (expect.int (Node.getNamedIndex child2)).toBe 1);
        test "getIndex returns correct values" (fun { expect; _ } ->
            let firstChild = Node.getNamedChild simpleNode 0 in
            let child1 = Node.getChild firstChild 0 in
            let child2 = Node.getChild firstChild 1 in
            (expect.int (Node.getIndex child1)).toBe 0;
            (expect.int (Node.getIndex child2)).toBe 1;
            (expect.int (Node.getNamedIndex child1)).toBe 0;
            (expect.int (Node.getNamedIndex child2)).toBe 0));
    describe "getDescendantForPointRange" (fun { test; _ } ->
        test "gets single line" (fun { expect; _ } ->
            let line2Node =
              Node.getDescendantForPointRange simpleNode 1 0 1 3
            in
            let ret = Node.toString line2Node in
            let startPoint = Node.getStartPoint line2Node in
            let endPoint = Node.getEndPoint line2Node in
            (expect.int (startPoint.line :> int)).toBe 1;
            (expect.int (startPoint.column :> int)).toBe 0;
            (expect.int (endPoint.line :> int)).toBe 1;
            (expect.int (endPoint.column :> int)).toBe 3;
            prerr_endline ("RET: " ^ ret);
            (expect.string ret).toEqual "(string (string_content))");
        describe "getStartPoint / getEndPoint" (fun { test; _ } ->
            test "returns correct value for root" (fun { expect; _ } ->
                let startPoint = Node.getStartPoint simpleNode in
                let endPoint = Node.getEndPoint simpleNode in
                (expect.int (startPoint.line :> int)).toBe 0;
                (expect.int (startPoint.column :> int)).toBe 0;
                (expect.int (endPoint.line :> int)).toBe 3;
                (expect.int (endPoint.column :> int)).toBe 0));
        describe "isError / hasError" (fun { test; _ } ->
            test "hasError returns false for no errors" (fun { expect; _ } ->
                (expect.bool (Node.hasError simpleNode)).toBe false);
            test "hasError turns true when there are errors"
              (fun { expect; _ } ->
                prerr_endline ("ERROR: " ^ Node.toString errorNode);
                (expect.bool (Node.hasError errorNode)).toBe true);
            test "isError returns true only for the error node"
              (fun { expect; _ } ->
                (expect.bool (Node.isError errorNode)).toBe false;
                let firstChild = Node.getNamedChild errorNode 0 in
                let firstGrandChild = Node.getNamedChild firstChild 0 in
                (expect.bool (Node.isError firstChild)).toBe false;
                (expect.bool (Node.isError firstGrandChild)).toBe true;
                prerr_endline ("GC: " ^ Node.toString firstGrandChild)))))
