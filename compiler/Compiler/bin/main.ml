open ToyClib
open Ast
open Semantic
open Codegen

(* 解析标准输入流为AST *)
let parse_stdin () : comp_unit =
  let lexbuf = Lexing.from_channel stdin in
  try 
    Parser.comp_unit Lexer.token lexbuf
  with
  | Lexer.LexError msg -> 
      Printf.eprintf "Lexer error: %s\n" msg;
      exit 1
  | Parsing.Parse_error ->
      Printf.eprintf "Parser error\n";
      exit 1

(* 主逻辑函数 *)
let run_program () =
  let ast = parse_stdin () in
  try analyze ast
  with SemanticError msg ->
    Printf.eprintf "Semantic error: %s\n" msg;

  
  let asm_code = compile ast in
  print_string asm_code

(* 添加C兼容的main函数 *)
let () =
  if not !Sys.interactive then
    run_program ()
