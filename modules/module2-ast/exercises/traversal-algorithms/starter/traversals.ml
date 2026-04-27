
(* traversals.ml - AST traversal algorithms exercise.
   Implement three classic tree traversal strategies on the AST:
   pre-order (depth-first), post-order (depth-first), and
   breadth-first (level-order).

   Each function walks a list of statements and collects a string label
   for every node visited. Labels should look like:
     Statements: "Assign", "If", "While", "Return", "Print", "Block"
     Expressions: "IntLit(3)", "BoolLit(true)", "Var(x)", "BinOp(+)",
                  "UnaryOp(-)", "Call(f)"
*)


open Shared_ast.Ast_types

 type node =
    | S of stmt
    | E of expr

(** Helper: produce a string label for a single expression node.
    Examples: IntLit(3), BoolLit(true), Var(x), BinOp(+), UnaryOp(-), Call(f) *)
let label_of_expr (_e : expr) : string =
  match _e with
  | IntLit n -> "IntLit(" ^ string_of_int n ^ ")"
  | BoolLit b -> "BoolLit(" ^ string_of_bool b ^ ")"
  | Var x -> "Var(" ^ x ^ ")"
  | BinOp (op, _, _) ->
      let op_str =
        match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Eq -> "=="
        | Neq -> "!="
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">="
        | And -> "&&"
        | Or -> "||"
      in
      "BinOp(" ^ op_str ^ ")"

      | UnaryOp (op, _) ->
      let op_str =
        match op with
        | Neg -> "-"
        | Not -> "!"
      in
      "UnaryOp(" ^ op_str ^ ")"
  | Call (f, _) -> "Call(" ^ f ^ ")"

(** Helper: produce a string label for a single statement node.
    Examples: "Assign", "If", "While", "Return", "Print", "Block" *)
let label_of_stmt (_s : stmt) : string =
  match _s with
  | Assign _ -> "Assign"
  | If _ -> "If"
  | While _ -> "While"
  | Return _ -> "Return"
  | Print _ -> "Print"
  | Block _ -> "Block"

(** Pre-order depth-first traversal.
    Visit the current node FIRST, then recurse into its children
    left-to-right.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["Assign"; "BinOp(+)"; "IntLit(1)"; "IntLit(2)"]

    Hint: write a mutual recursion with helpers for expr and stmt lists. *)
let pre_order (_stmts : stmt list) : string list =
  let rec visit_stmt s =
    let here = [label_of_stmt s] in
    match s with
    | Assign (_, e) ->
        here @ visit_expr e

    | If (cond, tbranch, fbranch) ->
        here
        @ visit_expr cond
        @ visit_stmts tbranch
        @ visit_stmts fbranch

    | While (cond, body) ->
        here
        @ visit_expr cond
        @ visit_stmts body

        | Return None ->
        here
    | Return (Some e) ->
        here @ visit_expr e

    | Print e ->
        here @ List.concat_map visit_expr e

    | Block stmts ->
        here @ visit_stmts stmts

        and visit_expr e =
    let here = [label_of_expr e] in
    match e with
    | BinOp (_, e1, e2) ->
        here @ visit_expr e1 @ visit_expr e2
    | UnaryOp (_, e1) ->
        here @ visit_expr e1
    | Call (_, args) ->
        here @ List.concat_map visit_expr args
    | _ ->
        here

  and visit_stmts ss =
    List.concat_map visit_stmt ss
  in
  visit_stmts _stmts


(** Post-order depth-first traversal.
    Recurse into children FIRST, then visit the current node.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["IntLit(1)"; "IntLit(2)"; "BinOp(+)"; "Assign"]

    Hint: same structure as pre_order but emit the label at the end. *)
let post_order (_stmts : stmt list) : string list =
  let rec visit_stmt s =
    let here = [label_of_stmt s] in
    match s with
    | Assign (_, e) ->
        visit_expr e @ here

    | If (cond, tbranch, fbranch) ->
        visit_expr cond
        @ visit_stmts tbranch
        @ visit_stmts fbranch
        @ here

    | While (cond, body) ->
        visit_expr cond
        @ visit_stmts body
        @ here

        | Return None ->
        here
    | Return (Some e) ->
        visit_expr e @ here

    | Print e ->
        List.concat_map visit_expr e @ here

    | Block stmts ->
        visit_stmts stmts @ here

  and visit_expr e =
    let here = [label_of_expr e] in
    match e with
    | BinOp (_, e1, e2) ->
        visit_expr e1 @ visit_expr e2 @ here
    | UnaryOp (_, e1) ->
        visit_expr e1 @ here
    | Call (_, args) ->
        List.concat_map visit_expr args @ here
    | _ ->

      here

  and visit_stmts ss =
    List.concat_map visit_stmt ss
  in
  visit_stmts _stmts

(** Breadth-first (level-order) traversal.
    Visit all nodes at depth d before any node at depth d+1.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["Assign"; "BinOp(+)"; "IntLit(1)"; "IntLit(2)"]
    (In this small case it happens to match pre-order, but differs on
     deeper trees with multiple siblings.)

    Hint: use the OCaml Queue module.
      1. Seed the queue with all top-level stmts.
      2. Dequeue a node, emit its label, enqueue its children.
      3. Repeat until the queue is empty.
    You will need a sum type or two queues to handle both stmt and expr
    nodes uniformly. *)
let bfs (_stmts : stmt list) : string list =
  let module Q = Queue in
  let q = Q.create () in

  let enqueue_stmt s = Q.add (S s) q in
  let enqueue_expr e = Q.add (E e) q in

  List.iter enqueue_stmt _stmts;

  let result = ref [] in
  while not (Q.is_empty q) do
    match Q.take q with
    | S s ->
        result := !result @ [label_of_stmt s];
        begin match s with
        | Assign (_, e) ->
            enqueue_expr e
        | If (cond, tbranch, fbranch) ->
            enqueue_expr cond;
            List.iter enqueue_stmt tbranch;
            List.iter enqueue_stmt fbranch
        | While (cond, body) ->
            enqueue_expr cond;
            List.iter enqueue_stmt body
        | Return None -> ()
        | Return (Some e) ->
            enqueue_expr e
        | Print e ->
            List.iter enqueue_expr e
        | Block stmts ->
            List.iter enqueue_stmt stmts
        end

        | E e ->
        result := !result @ [label_of_expr e];
        begin match e with
        | BinOp (_, e1, e2) ->
            enqueue_expr e1;
            enqueue_expr e2
        | UnaryOp (_, e1) ->
            enqueue_expr e1
        | Call (_, args) ->
            List.iter enqueue_expr args
        | _ -> ()
        end
  done;

  !result
