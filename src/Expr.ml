(* Simple expressions: syntax and semantics *)

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
             
(* The type for the expression. Note, in regular OCaml there is no "@type..." 
   notation, it came from GT. 
*)
@type expr =
  (* integer constant *) | Const of int
  (* variable         *) | Var   of string
  (* binary operator  *) | Binop of string * expr * expr with show

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

(* An example of a non-trivial state: *)                                                   
let s = update "x" 1 @@ update "y" 2 @@ update "z" 3 @@ update "t" 4 empty

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
    | _    -> failwith (Printf.sprintf "Unknown operator");;

(* Some testing; comment this definition out when submitting the solution. *)
(*let _ =*)
  (*List.iter*)
    (*(fun x ->*)
       (*try  Printf.printf "%s=%d\n" x @@ s x*)
       (*with Failure s -> Printf.printf "%s\n" s*)
    (*) ["x"; "a"; "y"; "z"; "t"; "b"]*)

(* Expression evaluator

     val eval : state -> expr -> int
 
   Takes a state and an expression, and returns the value of the expression in 
   the given state.
*)
let rec eval s e = match e with
    | Const n          -> n
    | Var x            -> s x
    | Binop (op, l, r) -> eval_op op (eval s l) (eval s r);;
