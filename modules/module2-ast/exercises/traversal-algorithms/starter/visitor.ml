(* visitor.ml - AST visitor pattern exercises.
   Implement two common visitor-style operations that walk the AST
   and accumulate information. *)
 
open Shared_ast.Ast_types

(** Count the number of each node type in a statement list.
    Returns an association list like:
      [("Assign", 3); ("IntLit", 5); ("BinOp", 2); ...]
    The keys are constructor names WITHOUT parameters (e.g., "IntLit"
    not "IntLit(3)"). Order does not matter.

    Hint:
      - Write recursive helpers for expr and stmt.
      - Use a mutable Hashtbl or a ref to a Map to accumulate counts,
        or thread an accumulator through the recursion.
      - Don't forget to count the node itself AND recurse into its
        children. *)
let count_nodes (_stmts : stmt list) : (string * int) list =
  let tbl = Hashtbl.create 32 in

  let bump name =
    let count = match Hashtbl.find_opt tbl name with
      | Some c -> c
      | None -> 0
    in
    Hashtbl.replace tbl name (count + 1)
  in
let rec count_expr e =
    match e with
    | IntLit _ ->
        bump "IntLit"
    | BoolLit _ ->
        bump "BoolLit"
    | Var _ ->
        bump "Var"
    | BinOp (_, e1, e2) ->
        bump "BinOp";
        count_expr e1;
        count_expr e2
    | Call (_, args) ->
        bump "Call";
        List.iter count_expr args
    | _ -> ()
  in

  let rec count_stmt s =
    match s with
    | Assign (_, e) ->
        bump "Assign";
        count_expr e
    | If (cond, then_branch, else_branch) ->
        bump "If";
        count_expr cond;
        List.iter count_stmt then_branch;
        List.iter count_stmt else_branch
    | While (cond, body) ->
        bump "While";
        count_expr cond;
        List.iter count_stmt body
    | Return None ->
        bump "Return"
    | Return (Some e) ->
        bump "Return";
        count_expr e
    | _ -> ()
    in

    List.iter count_stmt _stmts;
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

(* Evaluate a constant expression, returning Some int if the
    expression contains only integer literals and arithmetic operators,
    or None if it contains variables, booleans, calls, or comparison
    operators.

    Supported operators: Add, Sub, Mul, Div (integer division).
    Division by zero should return None.

    Examples:
      evaluate (IntLit 42)                        => Some 42
      evaluate (BinOp (Add, IntLit 1, IntLit 2))  => Some 3
      evaluate (BinOp (Add, IntLit 1, Var "x"))   => None
      evaluate (BoolLit true)                      => None

    Hint: use Option.bind or match on recursive results. *)

let rec evaluate e =
  match e with
  | IntLit n -> Some n

  | UnaryOp (Neg, e1) ->
      begin match evaluate e1 with
      | Some v -> Some (-v)
      | None -> None
      end

  | UnaryOp (Not, _) ->
      None

      | BinOp (op, e1, e2) ->
      begin match evaluate e1, evaluate e2 with
      | Some v1, Some v2 ->
          begin match op with
          | Add -> Some (v1 + v2)
          | Sub -> Some (v1 - v2)
          | Mul -> Some (v1 * v2)
          | Div ->
              if v2 = 0 then None else Some (v1 / v2)
          | _ -> None   
          end
      | _ -> None
      end

  | _ -> None
