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

let rec string_of_addr = function
	| Char i -> "#" ^ string_of_int i
	| Line i -> string_of_int i
	| ForwardRE re -> "/" ^ re ^ "/"
	| BackwardRE re -> "?" ^ re ^ "?"
	| Zero -> "0"
	| Dollar -> "$"
	| Dot -> "."
	| Plus (a1, a2) -> string_of_addr a1 ^ "+" ^ string_of_addr a2
	| Minus (a1, a2) -> string_of_addr a1 ^ "-" ^ string_of_addr a2
	| Comma (a1, a2) -> string_of_addr a1 ^ "," ^ string_of_addr a2
	| Semicolon (a1, a2) -> string_of_addr a1 ^ ";" ^ string_of_addr a2
	| Null -> ""

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

let string_of_ctl_msg = function
	| AddrEqDot -> "addr=dot\n"
	| Clean -> "clean\n"
	| Dirty -> "dirty\n"
	| Cleartag -> "cleartag\n"
	| Del -> "del\n"
	| Delete -> "delete\n"
	| DotEqAddr -> "dot=addr\n"
	| Dump cmd -> "dump " ^ cmd ^ "\n"
	| Dumpdir dirname -> "dumpdir " ^ dirname ^ "\n"
	| Get -> "get\n"
	| Limit -> "limit=addr\n"
	| Mark -> "mark\n"
	| Name name -> "name " ^ name ^ "\n"
	| Nomark -> "nomark\n"
	| Put -> "put\n"
	| Show -> "show\n"

let string_of_ctl_msgs msgs =
	String.concat "" (List.map string_of_ctl_msg msgs)