
let namespace = Sys.getenv "NAMESPACE"

module Addr = struct

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

let rec p = function
	| Char i -> "#" ^ string_of_int i
	| Line i -> string_of_int i
	| ForwardRE re -> "/" ^ re ^ "/"
	| BackwardRE re -> "?" ^ re ^ "?"
	| Zero -> "0"
	| Dollar -> "$"
	| Dot -> "."
	| Plus (a1, a2) -> p a1 ^ "+" ^ p a2
	| Minus (a1, a2) -> p a1 ^ "-" ^ p a2
	| Comma (a1, a2) -> p a1 ^ "," ^ p a2
	| Semicolon (a1, a2) -> p a1 ^ ";" ^ p a2
	| Null -> ""

end

module Ctl = struct

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

let p = function
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

let ps msgs =
	String.concat "" (List.map p msgs)

end

module Win = struct

type t = string
let compare : t -> t -> int = Pervasives.compare
let new_ : t = "new"
let p (t:t) :string = t
let r (t:string) :t = t

type hier =
	| Addr
	| Body
	| Ctl
	| Data
	| Errors
	| Event
	| Tag
	| Xdata

let string_of_hier = function
	| Addr -> "addr"
	| Body -> "body"
	| Ctl -> "ctl"
	| Data -> "data"
	| Errors -> "errors"
	| Event -> "event"
	| Tag -> "tag"
	| Xdata -> "xdata"

let path t h = t ^ "/" ^ string_of_hier h

let ls ?conn ?user () =
	let conn = match conn with
		| None -> O9pc.connect (Printf.sprintf "%s/acme" namespace)
		| Some conn -> conn
	in
	let fid = O9pc.attach conn ?user "" in
	let io = O9pc.fopen conn fid O9pc.oREAD in
	let data = O9pc.read conn fid io 4096l in
	let files = List.map (fun x -> x.Fcall.name) (O9pc.unpack_files data) in
	O9pc.clunk conn fid;
	let files = List.filter (fun w ->
		try ignore (int_of_string w); true
		with Failure "" -> false
		)
		files
	in
	files

let current () = Sys.getenv "winid"

end