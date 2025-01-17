open Ast

(* [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [subst e v x] is [e{v/x}]. *)
let rec subst e v x = match e with
  | Int _ | Bool _ -> e
  | Var y -> if y = x then v else e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, t, e1, e2) -> if y = x then Let (y, t, subst e1 v x, e2) else Let (y, t, subst e1 v x, subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)

(** [is value] is whether [e] is a value *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Var _ | Let _| Binop _ | If _ -> false

(** [eval_big e] is the [e ==> v] relation. *)
let rec eval (e : expr) : expr = match e with
  | Int _ | Bool _ -> e
  | Var _ -> failwith "Unbound variable"
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2
  | Let (x, _, e1, e2) -> subst e2 (eval e1) x |> eval
  | If (e1, e2, e3) -> eval_if e1 e2 e3

(** [eval_bop bop e1 e2] is the [e] such that [e1 bop e2 ==> e]. *)
and eval_bop bop e1 e2 = match bop, eval e1, eval e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith "Operator and operand type are mismatch"

and eval_if e1 e2 e3 = match eval e1 with
| Bool true -> eval e2
| Bool false -> eval e3
| _ -> failwith "Guard type must be bool"

(** [string_of_val e] converts [e] to a string. Requires: [e] is a value *)
let string_of_val (e : expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> failwith "precondition violated"

(** [interp s] interprets [s] by lexing and parsing it, evaluating it, and converting the result to a string *)
let interp (s : string) : string =
	s |> parse |> eval |> string_of_val
