(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Simple expressions: syntax and semantics *)
module Expr =
  struct

    (* The type for expressions. Note, in regular OCaml there is no "@type..."
       notation, it came from GT.
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* State: a partial map from variables to integer values. *)
    type state = string -> int

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Cast bool to int *)
    let to_int b = if b then 1 else 0

    (* Cast int to bool *)
    let from_int i = i != 0

    (* Cast (int->int->bool) to (int->int->int) *)
    let cast_op1 f = fun a b -> to_int (f a b)

    (* Cast (bool->bool->bool) to (int->int->int) *)
    let cast_op2 f = fun a b -> to_int (f (from_int a) (from_int b))

    (* Source operator into OCaml operator *)
    let eval_op op = match op with
        | "!!" -> cast_op2 ( || )
        | "&&" -> cast_op2 ( && )
        | "==" -> cast_op1 ( = )
        | "!=" -> cast_op1 ( <> )
        | "<=" -> cast_op1 ( <= )
        | "<"  -> cast_op1 ( < )
        | ">=" -> cast_op1 ( >= )
        | ">"  -> cast_op1 ( > )
        | "+"  -> ( + )
        | "-"  -> ( - )
        | "*"  -> ( * )
        | "/"  -> ( / )
        | "%"  -> ( mod )
        | _    -> failwith "Unknown operator";;

    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)
    let rec eval st e = match e with
        | Const n          -> n
        | Var var          -> st var
        | Binop (op, l, r) -> eval_op op (eval st l) (eval st r);;
  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list

    (* Exception where configuration has no input *)
    let no_input _ = failwith "Not enough input (Syntax)"

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval ((st, is, os) as cfg) stmt = match stmt with
        | Read var           -> (match is with
            | i :: is0 -> (Expr.update var i st, is0, os)
            | _        -> no_input ()
            )
        | Write expr         -> (st, is, Expr.eval st expr :: os)
        | Assign (var, expr) -> (Expr.update var (Expr.eval st expr) st, is, os)
        | Seq (fst, snd)     -> eval (eval cfg fst) snd
        ;;
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
