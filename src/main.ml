open Ast

let bop_err = "Operator and operand type mismatch"
let if_guard_err = "Guard of if must have type bool"
let if_branch_err = "Branches of if must have same type"
let unbound_var_err = "Unbound variable"

module type StaticEnvironment = sig
  (** [t] is the type of a static environment *)
  type t

  (** [empty] is the empty static environment *)
  val empty : t

  (** [lookup env x] gets the binding of [x] in [env]. Raises: [Failure] if [x] is not bound in [env] *)
  val lookup : t -> string -> typ

  (** [extend env x ty] is [env] extended with a binding of [x] to [ty] *)
  val extend : t -> string -> typ -> t
end

module StaticEnvironment : StaticEnvironment = struct
  type t = (string * typ) list

  let empty = []

  let lookup env x =
    try List.assoc x env
    with Not_found -> failwith unbound_var_err

  let extend env x ty =
    (x, ty) :: env
end

open StaticEnvironment

(** [typeof env e] is the type of [e] in static environment [env]. Raises: [Failure] if [e] is not well-typed in [env] *)
let rec typeof env = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> lookup env x
  | Binop (bop, e1, e2) -> typeof_bop env bop e1 e2
  | Let (x, t, e1, e2) -> typeof_let env x t e1 e2
  | If (e1, e2, e3) -> typeof_if env e1 e2 e3

and typeof_bop env bop e1 e2 =
  match bop, typeof env e1, typeof env e2 with
  | Add, TInt, TInt -> TInt
  | Mult, TInt, TInt -> TInt
  | Leq, TInt, TInt -> TBool
  | _ -> failwith bop_err

and typeof_let env x t e1 e2 =
  let t1 = typeof env e1 in
  if t1 = t then
    let env' = extend env x t1 in
    typeof env' e2
  else failwith "Type annotation error"

and typeof_if env e1 e2 e3 =
  match typeof env e1 with
  | TBool -> 
    let t2 = typeof env e2 in
    let t3 = typeof env e3 in
    if t2 = t3 then t2 else failwith if_branch_err
  | _ -> failwith if_guard_err

(** [typecheck e] checks whether [e] is well-typed in the empty static environment. Raises: [Failure] if not. *)
let typecheck e =
  let _ = typeof empty e in
  e

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
  | Var _ -> failwith unbound_var_err
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2
  | Let (x, _, e1, e2) -> subst e2 (eval e1) x |> eval
  | If (e1, e2, e3) -> eval_if e1 e2 e3

(** [eval_bop bop e1 e2] is the [e] such that [e1 bop e2 ==> e]. *)
and eval_bop bop e1 e2 = match bop, eval e1, eval e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

and eval_if e1 e2 e3 = match eval e1 with
| Bool true -> eval e2
| Bool false -> eval e3
| _ -> failwith if_guard_err

(** [string_of_val e] converts [e] to a string. Requires: [e] is a value *)
let string_of_val (e : expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> failwith "precondition violated"

(** [interp s] interprets [s] by lexing and parsing it, evaluating it, and converting the result to a string *)
let interp (s : string) : string =
	s |> parse |> typecheck |> eval |> string_of_val
