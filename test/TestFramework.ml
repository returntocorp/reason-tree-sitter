include Rely.Make (struct
  let config =
    Rely.TestFrameworkConfig.initialize
      { snapshotDir = "./__snapshots__"; projectDir = "" }
end)
