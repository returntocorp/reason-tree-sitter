open EditorCoreTypes
open Treesitter
open TestFramework

let () =
  describe "ArrayParser" (fun { describe; _ } ->
    describe "parse" (fun { test; _ } ->
        test "parses a single line array" (fun { expect; _ } ->
            let jsonParser = Parser.json () in
            let tree, _ =
              ArrayParser.parse jsonParser None [| "[1, \"2\"]" |]
            in
            let node = Tree.getRootNode tree in
            let ret = Node.toString node in
            prerr_endline ("RET: " ^ ret);
            (expect.string ret).toEqual
              "(value (array (number) (string (string_content))))");
        test "parses a multi-line array" (fun { expect; _ } ->
            let multiLineArray = [| "["; "1,"; "\"2\""; "]"; "" |] in
            let jsonParser = Parser.json () in
            let tree, _ = ArrayParser.parse jsonParser None multiLineArray in
            let node = Tree.getRootNode tree in
            let ret = Node.toString node in
            prerr_endline ("RET: " ^ ret);
            (expect.string ret).toEqual
              "(value (array (number) (string (string_content))))"));
    describe "incremental parse" (fun { test; _ } ->
        test "incrementally update single line" (fun { expect; _ } ->
            let jsonParser = Parser.json () in
            let _, baseline =
              ArrayParser.parse jsonParser None [| "[1, \"2\"]" |]
            in
            let update = [| "[1]" |] in
            let delta = ArrayParser.Delta.create baseline 0 1 update in
            let tree, _ =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) update
            in
            let node = Tree.getRootNode tree in
            let ret = Node.toString node in
            prerr_endline ("RET: " ^ ret);
            (expect.string ret).toEqual "(value (array (number)))");
        test "change single line" (fun { expect; _ } ->
            let start = [| "["; "1,"; "\"2\","; "3"; "]"; "" |] in
            let endv = [| "["; "1,"; "2,"; "3"; "]"; "" |] in
            let jsonParser = Parser.json () in
            let _, baseline = ArrayParser.parse jsonParser None start in
            let update = [| "2," |] in
            let delta = ArrayParser.Delta.create baseline 2 3 update in
            let tree, _ =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) endv
            in
            let node = Tree.getRootNode tree in
            let ret = Node.toString node in
            prerr_endline ("RET: " ^ ret);
            (expect.string ret).toEqual
              "(value (array (number) (number) (number)))");
        test "remove multiple lines" (fun { expect; _ } ->
            let start = [| "["; "1,"; "\"2\","; "3"; "]"; "" |] in
            let endv = [| "["; "]"; "" |] in
            let jsonParser = Parser.json () in
            let _, baseline = ArrayParser.parse jsonParser None start in
            let update = [||] in
            let delta = ArrayParser.Delta.create baseline 1 4 update in
            let tree, _ =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) endv
            in
            let node = Tree.getRootNode tree in
            let ret = Node.toString node in
            prerr_endline ("RET: " ^ ret);
            (expect.string ret).toEqual "(value (array))");
        test "add multiple lines" (fun { expect; _ } ->
            let start = [| "["; "]"; "" |] in
            let endv = [| "["; "1,"; "\"2\","; "3"; "]"; "" |] in
            let jsonParser = Parser.json () in
            let _, baseline = ArrayParser.parse jsonParser None start in
            let update = [| "1,"; "\"2\","; "3" |] in
            let delta = ArrayParser.Delta.create baseline 1 1 update in
            let tree, _ =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) endv
            in
            let node = Tree.getRootNode tree in
            let ret = Node.toString node in
            prerr_endline ("RET: " ^ ret);
            (expect.string ret).toEqual
              "(value (array (number) (string (string_content)) (number)))");
        test "update multiple lines" (fun { expect; _ } ->
            let start = [| "["; "1,"; "\"2\","; "3"; "]"; "" |] in
            let endv = [| "["; "\"1\","; "2,"; "\"3\""; "]" |] in
            let jsonParser = Parser.json () in
            let _, baseline = ArrayParser.parse jsonParser None start in
            let update = [| "\"1\","; "2,"; "\"3\"" |] in
            let delta = ArrayParser.Delta.create baseline 1 4 update in
            let tree, _ =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) endv
            in
            let node = Tree.getRootNode tree in
            let ret = Node.toString node in
            prerr_endline ("RET: " ^ ret);
            (expect.string ret).toEqual
              "(value (array (string (string_content)) (number) (string \
               (string_content))))");
        let tokenRangeMatches ~token ~(range : Range.t) () =
          let startPosition = Syntax.Token.getPosition token in
          let endPosition = Syntax.Token.getEndPosition token in
          range.start.line = startPosition.line
          && range.start.column = startPosition.column
          && range.stop.line = endPosition.line
          && range.stop.column = endPosition.column
        in
        test "token positions are preserved when deleting a line"
          (fun { expect; _ } ->
            let start = [| "["; ""; "]" |] in
            let endv = [| "["; "]" |] in
            let jsonParser = Parser.json () in
            let _, baseline = ArrayParser.parse jsonParser None start in
            let update = [||] in
            let delta = ArrayParser.Delta.create baseline 1 2 update in
            let tree, _ =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) endv
            in
            let node = Tree.getRootNode tree in
            let range =
              Range.create
                ~start:(Location.create ~line:Index.zero ~column:Index.zero)
                ~stop:
                  (Location.create
                     ~line:
                       (let open Index in
                       zero + 3)
                     ~column:Index.zero)
            in
            prerr_endline "-----START-------";
            let getTokenName = Syntax.createArrayTokenNameResolver endv in
            let tokens = Syntax.getTokens ~getTokenName ~range node in
            (* Validate tokens aren't shifted when deleting a row *)
            let leftBracket = List.nth tokens 0 in
            (expect.bool
               (tokenRangeMatches ~token:leftBracket
                  ~range:
                    (Range.create
                       ~start:
                         (Location.create ~line:Index.zero ~column:Index.zero)
                       ~stop:
                         (Location.create ~line:Index.zero
                            ~column:
                              (let open Index in
                              zero + 1)))
                  ()))
              .toBe true;
            (expect.string (Syntax.Token.getName leftBracket)).toEqual "\"[\"";
            let rightBracket = List.nth tokens 1 in
            (expect.bool
               (tokenRangeMatches ~token:rightBracket
                  ~range:
                    (Range.create
                       ~start:
                         (Location.create
                            ~line:
                              (let open Index in
                              zero + 1)
                            ~column:Index.zero)
                       ~stop:
                         (Location.create
                            ~line:
                              (let open Index in
                              zero + 1)
                            ~column:
                              (let open Index in
                              zero + 1)))
                  ()))
              .toBe true;
            (expect.string (Syntax.Token.getName rightBracket)).toEqual "\"]\"";
            List.iter (fun t -> prerr_endline (Syntax.Token.show t)) tokens);
        test "token positions are preserved when adding a line"
          (fun { expect; _ } ->
            let start = [| "["; "]" |] in
            let endv = [| "["; ""; "]" |] in
            let jsonParser = Parser.json () in
            let _, baseline = ArrayParser.parse jsonParser None start in
            let update = [| "" |] in
            let delta = ArrayParser.Delta.create baseline 1 1 update in
            let tree, _ =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) endv
            in
            let node = Tree.getRootNode tree in
            let getTokenName _ = "" in
            let range =
              Range.create
                ~start:(Location.create ~line:Index.zero ~column:Index.zero)
                ~stop:
                  (Location.create
                     ~line:
                       (let open Index in
                       zero + 3)
                     ~column:Index.zero)
            in
            prerr_endline "-----START-------";
            let tokens = Syntax.getTokens ~getTokenName ~range node in
      // Validate tokens aren't shifted when deleting a row
            let leftBracket = List.nth tokens 0 in
            (expect.bool
               (tokenRangeMatches ~token:leftBracket
                  ~range:
                    (Range.create
                       ~start:
                         (Location.create ~line:Index.zero ~column:Index.zero)
                       ~stop:
                         (Location.create ~line:Index.zero
                            ~column:
                              (let open Index in
                              zero + 1)))
                  ()))
              .toBe true;
            let rightBracket = List.nth tokens 1 in
            (expect.bool
               (tokenRangeMatches ~token:rightBracket
                  ~range:
                    (Range.create
                       ~start:
                         (Location.create
                            ~line:
                              (let open Index in
                              zero + 2)
                            ~column:Index.zero)
                       ~stop:
                         (Location.create
                            ~line:
                              (let open Index in
                              zero + 2)
                            ~column:
                              (let open Index in
                              zero + 1)))
                  ()))
              .toBe true;
            List.iter (fun t -> prerr_endline (Syntax.Token.show t)) tokens);
        test "token positions are preserved when modifying a line"
          (fun { expect; _ } ->
            let start = [| "["; ""; "]" |] in
            let endv = [| "["; "a"; "]" |] in
            let jsonParser = Parser.json () in
            let _, baseline = ArrayParser.parse jsonParser None start in
            let update = [| "a" |] in
            let delta = ArrayParser.Delta.create baseline 1 2 update in
            let tree, _ =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) endv
            in
            let node = Tree.getRootNode tree in
            let range =
              Range.create
                ~start:(Location.create ~line:Index.zero ~column:Index.zero)
                ~stop:
                  (Location.create
                     ~line:
                       (let open Index in
                       zero + 3)
                     ~column:Index.zero)
            in
            let getTokenName = Syntax.createArrayTokenNameResolver endv in
            prerr_endline "-----START-------";
            let tokens = Syntax.getTokens ~getTokenName ~range node in
      // Validate tokens aren't shifted when deleting a row
            let leftBracket = List.nth tokens 0 in
            (expect.bool
               (tokenRangeMatches ~token:leftBracket
                  ~range:
                    (Range.create
                       ~start:
                         (Location.create ~line:Index.zero ~column:Index.zero)
                       ~stop:
                         (Location.create ~line:Index.zero
                            ~column:
                              (let open Index in
                              zero + 1)))
                  ()))
              .toBe true;
            (expect.string (Syntax.Token.getName leftBracket)).toEqual "\"[\"";
            let rightBracket = List.nth tokens 2 in
            (expect.bool
               (tokenRangeMatches ~token:rightBracket
                  ~range:
                    (Range.create
                       ~start:
                         (Location.create
                            ~line:
                              (let open Index in
                              zero + 2)
                            ~column:Index.zero)
                       ~stop:
                         (Location.create
                            ~line:
                              (let open Index in
                              zero + 2)
                            ~column:
                              (let open Index in
                              zero + 1)))
                  ()))
              .toBe true;
            (expect.string (Syntax.Token.getName rightBracket)).toEqual "\"]\"";
            List.iter (fun t -> prerr_endline (Syntax.Token.show t)) tokens);
        test "regression test: multiple delta updates" (fun { expect; _ } ->
            let jsonParser = Parser.json () in
            let start = [| "["; "]" |] in
            let _, baseline = ArrayParser.parse jsonParser None start in
            let delta1 = [| "\"a\","; "\"b\"," |] in
            let end1 = [| "["; "\"a\","; "\"b\","; "]" |] in
            let delta = ArrayParser.Delta.create baseline 1 1 delta1 in
            let _, baseline =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) end1
            in
            let delta2 = [| "" |] in
            let end2 = [| "["; "\"a\","; "\"b\","; ""; "]" |] in
            let delta = ArrayParser.Delta.create baseline 3 3 delta2 in
            let _, baseline =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) end2
            in
            let delta3 = [| "\"" |] in
            let end3 = [| "["; "\"a\","; "\"b\","; "\""; "]" |] in
            let delta = ArrayParser.Delta.create baseline 3 4 delta3 in
            let tree, _ =
              ArrayParser.parse jsonParser (Some delta [@explicit_arity]) end3
            in
            let node = Tree.getRootNode tree in
            let rangeLine2 =
              Range.create
                ~start:(Location.create ~line:Index.zero ~column:Index.zero)
                ~stop:
                  (Location.create
                     ~line:
                       (let open Index in
                       zero + 6)
                     ~column:Index.zero)
            in
            let getTokenName = Syntax.createArrayTokenNameResolver end3 in
            let tokens =
              Syntax.getTokens ~getTokenName ~range:rangeLine2 node
            in
            (* There should be these tokens at this point:
               Token(0,0 - 0,1:(0:array).(0:value)|"[")
               Token(1,0 - 1,1:(0:string).(0:array).(0:value)|""")
               Token(1,1 - 1,2:(0:string_content).(0:string).(0:array).(0:value)|"a")
               Token(1,2 - 1,3:(0:string).(0:array).(0:value)|""")
               Token(1,3 - 1,4:(0:array).(0:value)|",")
               Token(2,0 - 2,1:(1:string).(0:array).(0:value)|""")
               Token(2,1 - 2,2:(0:string_content).(1:string).(0:array).(0:value)|"b")
               Token(2,2 - 2,3:(1:string).(0:array).(0:value)|""")
               Token(2,3 - 2,4:(0:array).(0:value)|",")
               Token(3,0 - 3,1:(2:string).(0:array).(0:value)|""")
               Token(3,1 - 3,1:(2:string).(0:array).(0:value)|)
               Token(4,0 - 4,1:(0:array).(0:value)|"]")
             *)

           (* List.iter (fun t -> prerr_endline (Syntax.Token.show t) tokens; *)

           (* With the byte offest bug, we'd instead crash when resolving
              tokens *)
            (expect.int (List.length tokens)).toBe 12)))
