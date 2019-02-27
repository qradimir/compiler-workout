open GT

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Exception where configuration has empty stack *)
let empty_stack _ = failwith "Not enough arguments on stack"

(* Exception where configuration has no input *)
let no_input = Syntax.Stmt.no_input

(* Instruction interpreter *)
let eval_insn (stack, ((st, is, os) as state)) insn = match insn with
    | BINOP op -> (match stack with
        | fst :: snd :: stack' -> ((Syntax.Expr.eval_op op snd fst) :: stack', state)
        | _                    -> empty_stack ()
        )
    | CONST i  -> (i :: stack, state)
    | READ     -> (match is with
        | i :: is' -> (i :: stack, (st, is', os))
        | _        -> no_input ()
        )
    | WRITE    -> (match stack with
        | o :: stack' -> (stack', (st, is, os @ [o]))
        | _           -> empty_stack ()
        )
    | LD var -> (st var :: stack, state)
    | ST var -> (match stack with
        | v :: stack' -> (stack', (Syntax.Expr.update var v st, is, os))
        | _           -> empty_stack ()
        )
    ;;

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let eval config prg = List.fold_left eval_insn config prg

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Expression complier into stack machine instructions *)
let rec compile_expr expr = let open Syntax.Expr in match expr with
    | Const n          -> [CONST n]
    | Var var          -> [LD var]
    | Binop (op, l, r) -> compile_expr l @ compile_expr r @ [BINOP op]
    ;;

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile stmt = let open Syntax.Stmt in match stmt with
    | Read var           -> [READ; ST var]
    | Write expr         -> compile_expr expr @ [WRITE]
    | Assign (var, expr) -> compile_expr expr @ [ST var]
    | Seq (fst, snd)     -> compile fst @ compile snd
    ;;
