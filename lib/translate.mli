type exp = unit
type level
type access

type new_level_params = {
  parent: level;
  name: Temp.label;
  formals: bool list
}

val outermost : level
val new_level : new_level_params -> level
val formals : level -> access list
val all_local : level -> bool -> access
