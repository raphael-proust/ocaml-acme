
module Addr : sig

type re = string

(*As per sam(1) *)
type t =
	| Char of int
	| Line of int
	| ForwardRE of re
	| BackwardRE of re
	| Zero
	| Dollar
	| Dot
	| Plus of t * t
	| Minus of t * t
	| Comma of t * t
	| Semicolon of t * t
	| Null

val p: t -> string

end

module Ctl : sig

type command = string

type t =
	| AddrEqDot
	| Clean
	| Dirty
	| Cleartag
	| Del
	| Delete
	| DotEqAddr
	| Dump of command
	| Dumpdir of string
	| Get
	| Limit
	| Mark
	| Name of string
	| Nomark
	| Put
	| Show

val p: t -> string
val ps: t list -> string

end

module Win : sig

type t
val compare: t -> t -> int
val p: t -> string
val r: string -> t
val new_: t

type hier =
	| Addr
	| Body
	| Ctl
	| Data
	| Errors
	| Event
	| Tag
	| Xdata

val path: t -> hier -> string

val ls: ?conn:O9pc.t -> ?user:string -> unit -> t list
val current: unit -> t

end