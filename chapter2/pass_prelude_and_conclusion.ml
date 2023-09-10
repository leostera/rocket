open X86

let frame_size = 8

let prelude info =
  let locals = List.length info.locals * frame_size in
  [
    ( "_main",
      [
        Pushq (Reg Rbp);
        Movq (Reg Rsp, Reg Rbp);
        Subq (Int locals, Reg Rsp);
        Jmp "_start";
      ] );
    ("_conclusion", [ Addq (Int locals, Reg Rsp); Popq (Reg Rbp); Reqt ]);
  ]

let run { info; labels } = { info; labels = labels @ prelude info }
