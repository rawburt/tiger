type temp
val newtemp : unit -> temp

module Table : Map.S with type key = temp

val makestring : temp -> string

type label = Symbol.symbol
val newlabel : unit -> label
val namedlabel : string -> label
