open TestFramework

let () =
  describe "Describe" (fun { test; _ } ->
    test "test" (fun { expect } -> (expect.int 0).toBe 0))
