open Ast

(** [Env] is a module to help with environments, which are maps that have strings as keys*)
module Env = Map.Make(String)

let empty_env = Env.empty

(** [env] is the type of an environment, which maps a string to a value*)
type env = value Env.t

(** [value] is the type of a value *)
and value =
  | VInt of int
  | VBool of bool

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [subst e v x] is [e{v/x}]. *)
let rec subst e v x = match e with
  | Int _ | Bool _ -> e
  | Var y -> if y = x then v else e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) -> if y = x then Let (y, subst e1 v x, e2) else Let (y, subst e1 v x, subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)


(** [is value] is whether [e] is a value *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Var _ | Let _| Binop _ | If _ -> false

(** [step] is a [-->] relation, a single step of evalutaion of [e] *)
let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Does't step"
  | Var _ -> failwith "Unbound variable"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
    step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith "Guard of 'if' must be bool"
  | If (e1, e2, e3) -> If (step e1, e2, e3)

(** [step_bop bop v1 v2] implements the primitive operation [v1 bop v2]. Requires: [v1] and [v2] are both values *)
and step_bop bop v1 v2 = match bop, v1, v2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith "Operator and operand type mismatch"
  
(** [eval_small e] is the [e -->* v] relation. That is, keep applying [step] until a value is produced.  *)
let rec eval_small (e : expr) : expr =
  if is_value e then e
  else e |> step |> eval_small

(** [eval_big env e] is the [v] such that [<env, e> ==> v]. *)
let rec eval_big (env: env) (e : expr) : value = match e with
  | Int i -> VInt i 
  | Bool b -> VBool b
  | Var x -> eval_var env x
  | Binop (bop, e1, e2) -> eval_bop env bop e1 e2
  | Let (x, e1, e2) -> eval_let env x e1 e2
  | If (e1, e2, e3) -> eval_if env e1 e2 e3


(** [eval_var env x] is the [v] such that [<env, x> ==> v]. *)
and eval_var env x =
  try Env.find x env
  with Not_found -> failwith "Unbound variable"

(** [eval_bop env bop e1 e2] is the [v] such that [<env, e1 bop e2> ==> v]. *)
and eval_bop env bop e1 e2 = match bop, eval_big env e1, eval_big env e2 with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Mult, VInt a, VInt b -> VInt (a * b)
  | Leq, VInt a, VInt b -> VBool (a <= b)
  | _ -> failwith "Operator and operand type are mismatch"

(** [eval_let env x e1 e2] is the [v] such that [<env, let x = e1 in e2> ==> v]. *)
and eval_let env x e1 e2 =
  let v1 = eval_big env e1 in
  let env' = Env.add x v1 env in
  eval_big env' e2

(** [eval_if env e1 e2 e3] is the [v] such that [<env, if e1 then e2 else e3> ==> v]. *)
and eval_if env e1 e2 e3 = match eval_big env e1 with
| VBool true -> eval_big env e2
| VBool false -> eval_big env e3
| _ -> failwith "Guard type must be bool"

(** [string_of_val v] converts [v] to a string *)
let string_of_val (v : value) : string =
  match v with
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b

(** [interp s] interprets [s] by lexing and parsing it, evaluating it, and converting the result to a string *)
let interp_big (s : string) : string =
	s |> parse |> eval_big empty_env |> string_of_val

