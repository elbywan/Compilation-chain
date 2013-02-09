open WillowAST
open WillowInterpreter

let name = "willow"

type input =
  | Ast    of ast
  | String of string

let parse_file filename =
  SyntacticAnalysis.process
    ~lexer_init: (fun filename -> Lexing.from_channel (open_in filename))
    ~lexer_fun: WillowLexer.main
    ~parser_fun: WillowParser.toplevel_expression
    ~input: filename

let dispatch_between_expression_and_declaration_regexp =
  Str.regexp "[' ' '\009' '\012' '\n']*fun"

let is_declaration str =
  Str.string_match dispatch_between_expression_and_declaration_regexp str 0

let parse_string str =
  match Languages.parse_interactive_loop_directive str with
    | None ->
        if is_declaration str then
          Ast (Declaration 
                 (SyntacticAnalysis.process
                    ~lexer_init: Lexing.from_string
                    ~lexer_fun: WillowLexer.main
                    ~parser_fun: WillowParser.toplevel_declaration
                    ~input: str))
        else
          let e = SyntacticAnalysis.process
            ~lexer_init: Lexing.from_string
            ~lexer_fun: WillowLexer.main
            ~parser_fun: WillowParser.toplevel_expression
            ~input: str
          in
            Ast (Expression e)

    | Some ("load", [filename]) ->
        let e = 
          SyntacticAnalysis.process
            ~lexer_init: Misc.LexingUtils.from_filename
            ~lexer_fun: WillowLexer.main
            ~parser_fun: WillowParser.compilation_unit
            ~input: filename
        in
          Ast (Program e)

    | Some (directive, arguments) ->
        String
          (Printf.sprintf "=> %s [ %s ]%!" directive
             (String.concat " " arguments))

let eval ast runtime =
  match ast with
    | Ast ast ->
        let runtime, r = 
          WillowInterpreter.eval runtime ast 
        in
          (runtime, String (String.concat "\n" r))
    | s -> (runtime, s)

let print runtime s =
  match s with
    | String s -> s
    | Ast (Expression e) -> 
        Misc.FormatUtils.as_string WillowPrinter.expression e
    | Ast (Program p) -> 
        Misc.FormatUtils.as_string WillowPrinter.program p
    | Ast (Declaration e) -> 
        Misc.FormatUtils.as_string WillowPrinter.function_declaration e

let interaction_system = Languages.mk_language_is parse_string print eval

let translate_ast = function
  | Clovis.Ast (_, e) ->
      Ast (Program (WillowFromClovis.translate e))
  | _ -> 
      assert false

let init () =
    Languages.declare_language name interaction_system;
    Languages.declare_translation 
      Clovis.name name 
      Clovis.interaction_system interaction_system 
      translate_ast 
