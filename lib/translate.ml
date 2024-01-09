type exp = unit
type level = {
  parent: level option;
  frame: Frame.frame
}
type access = level * Frame.access

type new_level_params = {
  parent: level;
  name: Temp.label;
  formals: bool list
}

let outermost =
  let frame = Frame.new_frame
    {
      name = Temp.newlabel ();
      formals = [];
    }
  in
  { parent = None; frame }

let new_level params =
  (* add true to the formals to create the static link *)
  let frame = Frame.new_frame
    {
      name = params.name;
      formals = true :: params.formals
    }
  in
  { parent = Some params.parent; frame }

let formals level =
  List.map
    (fun access -> (level, access))
    (Frame.formals level.frame)

let all_local level escapes =
  (level, Frame.alloc_local level.frame escapes)
