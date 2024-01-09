type frame
type access

type new_frame_params = {
    name: Temp.label;
    formals: bool list;
}

val new_frame : new_frame_params -> frame
val formals : frame -> access list
val alloc_local : frame -> bool -> access
