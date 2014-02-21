type re = string

(*As per sam(1) *)
type addr =
	| Char of int
	| Line of int
	| ForwardRE of re
	| BackwardRE of re
	| Zero
	| Dollar
	| Dot
	| Plus of addr * addr
	| Minus of addr * addr
	| Comma of addr * addr
	| Semicolon of addr * addr
	| Null

val string_of_addr: addr -> string

type command = string
type dirname = string

type ctl_msg =
	| AddrEqDot
	| Clean
	| Dirty
	| Cleartag
	| Del
	| Delete
	| DotEqAddr
	| Dump of command
	| Dumpdir of dirname
	| Get
	| Limit
	| Mark
	| Name of string
	| Nomark
	| Put
	| Show

val string_of_ctl_msg: ctl_msg -> string
val string_of_ctl_msgs: ctl_msg list -> string