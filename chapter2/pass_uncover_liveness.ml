open X86_live

let block_liveness instrs = List.map (fun blk -> blk, { reads=[]; writes=[] } ) instrs

let run : X86_var.program -> X86_live.program =
 fun { info; labels } ->
  {
    info;
    labels = List.map (fun (lbl, block) -> (lbl, block_liveness block)) labels;
  }
