(* doing MIPS frame *)

type access =
  | InFrame of int
  | InReg of Temp.temp

type frame = {
  formals: access list;
  name: Temp.label;
  locals: int ref
}

type new_frame_params = {
    name: Temp.label;
    formals: bool list;
}

let new_frame {name; formals} =
  let in_frame = ref 0 in
  let build_frame_formal formal =
    if formal
      then begin
        let i = !in_frame in
        in_frame := i + 1;
        InFrame i
      end else
        InReg (Temp.newtemp ())
  in
  {
    formals = List.map build_frame_formal formals;
    name;
    locals = ref 0
  }

let alloc_local frame escapes =
  if escapes
    then begin
      frame.locals := !(frame.locals) + 1;
      (* MIPS word size is 4 bytes *)
      InFrame(!(frame.locals) * -4)
    end else
      (* MIPS calling convention passes 4 arguments in registers and the rest on the stack *)
      InReg (Temp.newtemp ())

let formals ({formals; _}:frame) = formals
