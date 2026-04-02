(** CFG Construction Exercises.

    Each function below takes a list of AST statements and returns a CFG
    whose shape matches a specific control-flow pattern.

    Students: implement the functions marked with TODO.

    General approach for each exercise:
    1. Create the basic blocks with [Cfg.create_block].
    2. Put them into a [Cfg.StringMap] keyed by label.
    3. Build the initial [Cfg.cfg] record with entry, exit_label, and blocks.
    4. Use [Cfg.add_edge] to wire up the control flow edges.

    The ENTRY and EXIT blocks are always empty (no statements). *)

open Shared_ast.Ast_types

(** Build a CFG for straight-line (sequential) code.

    Expected shape:

      ENTRY --> B1 --> EXIT

    All statements go into a single block B1.

    Example input:
      [ Assign ("x", IntLit 1);
        Assign ("y", IntLit 2);
        Assign ("z", BinOp (Add, Var "x", Var "y")) ]

    @param stmts  A flat list of statements with no branches or loops. *)
let build_cfg_sequential (stmts : stmt list) : Cfg.cfg =
  let entry = Cfg.create_block "ENTRY" [] in
  let b1    = Cfg.create_block "B1" stmts in
  let exit  = Cfg.create_block "EXIT" [] in

  let blocks =
    Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY" entry
    |> Cfg.StringMap.add "B1" b1
    |> Cfg.StringMap.add "EXIT" exit
  in

  let cfg = {
    Cfg.entry = "ENTRY";
    exit_label = "EXIT";
    blocks;
  } in

  
let cfg = Cfg.add_edge cfg "ENTRY" "B1" in
let cfg = Cfg.add_edge cfg "B1" "EXIT" in
cfg

let build_cfg_ifelse (stmts : stmt list) : Cfg.cfg =
let rec split acc = function
    | [] -> failwith "No If statement found"
    | If (_, then_s, else_s) :: rest ->
        (List.rev acc, then_s, else_s, rest)
    | s :: rest ->
        split (s :: acc) rest
  in
  let (pre, then_s, else_s, post) = split [] stmts in

  let entry  = Cfg.create_block "ENTRY" [] in
  let b_cond = Cfg.create_block "B_cond" pre in
  let b_then = Cfg.create_block "B_then" then_s in
  let b_else = Cfg.create_block "B_else" else_s in
  let b_join = Cfg.create_block "B_join" post in
  let exit   = Cfg.create_block "EXIT" [] in

  let blocks =
    Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY" entry
    |> Cfg.StringMap.add "B_cond" b_cond
    |> Cfg.StringMap.add "B_then" b_then
    |> Cfg.StringMap.add "B_else" b_else
    |> Cfg.StringMap.add "B_join" b_join
    |> Cfg.StringMap.add "EXIT" exit
  in

  let cfg = {
    Cfg.entry = "ENTRY";
    exit_label = "EXIT";
    blocks;
  } in


  let cfg = Cfg.add_edge cfg "ENTRY" "B_cond" in
let cfg = Cfg.add_edge cfg "B_cond" "B_then" in
let cfg = Cfg.add_edge cfg "B_cond" "B_else" in
let cfg = Cfg.add_edge cfg "B_then" "B_join" in
let cfg = Cfg.add_edge cfg "B_else" "B_join" in
let cfg = Cfg.add_edge cfg "B_join" "EXIT" in
cfg

let build_cfg_while (stmts : stmt list) : Cfg.cfg =
  let rec split acc = function
    | [] -> failwith "No While statement found"
    | While (_, body) :: rest ->
        (List.rev acc, body, rest)
    | s :: rest ->
        split (s :: acc) rest
  in
  let (pre, body, post) = split [] stmts in

  let entry  = Cfg.create_block "ENTRY" [] in
  let b_pre  = Cfg.create_block "B_pre" pre in
  let b_cond = Cfg.create_block "B_cond" [] in
  let b_body = Cfg.create_block "B_body" body in
  let b_post = Cfg.create_block "B_post" post in
  let exit   = Cfg.create_block "EXIT" [] in

  let blocks =
    Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY" entry
    |> Cfg.StringMap.add "B_pre" b_pre
    |> Cfg.StringMap.add "B_cond" b_cond
    |> Cfg.StringMap.add "B_body" b_body
    |> Cfg.StringMap.add "B_post" b_post
    |> Cfg.StringMap.add "EXIT" exit
  in

  let cfg = {
    Cfg.entry = "ENTRY";
    exit_label = "EXIT";
    blocks;
  } in

let cfg = Cfg.add_edge cfg "ENTRY" "B_pre" in
let cfg = Cfg.add_edge cfg "B_pre" "B_cond" in
let cfg = Cfg.add_edge cfg "B_cond" "B_body" in
let cfg = Cfg.add_edge cfg "B_cond" "B_post" in
let cfg = Cfg.add_edge cfg "B_body" "B_cond" in
let cfg = Cfg.add_edge cfg "B_post" "EXIT" in
cfg
