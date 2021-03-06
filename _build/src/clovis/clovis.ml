open ClovisAST

let name = "clovis"

type input =
  | Ast    of declaration
  | String of string

let parse_file filename =
  SyntacticAnalysis.process
    ~lexer_init: (fun filename -> Lexing.from_channel (open_in filename))
    ~lexer_fun: ClovisLexer.main
    ~parser_fun: ClovisParser.toplevel_expression
    ~input: filename

let dispatch_between_expression_and_declaration_regexp =
  Str.regexp "[' ' '\009' '\012' '\n']*val"

let is_declaration str =
  Str.string_match dispatch_between_expression_and_declaration_regexp str 0

let parse_string str =
  match Languages.parse_interactive_loop_directive str with
    | None ->
        if is_declaration str then
          Ast (SyntacticAnalysis.process
                 ~lexer_init: Lexing.from_string
                 ~lexer_fun: ClovisLexer.main
                 ~parser_fun: ClovisParser.toplevel_declaration
                 ~input: str)
        else
          let e = SyntacticAnalysis.process
            ~lexer_init: Lexing.from_string
            ~lexer_fun: ClovisLexer.main
            ~parser_fun: ClovisParser.toplevel_expression
            ~input: str
          in
            Ast ((ClovisIdentifier.fresh (), e))

    | Some ("load", [filename]) ->
        let e = 
          SyntacticAnalysis.process
            ~lexer_init: Misc.LexingUtils.from_filename
            ~lexer_fun: ClovisLexer.main
            ~parser_fun: ClovisParser.toplevel_expression
            ~input: filename
        in
          Ast (ClovisIdentifier.fresh (), e)

    | Some (directive, arguments) ->
        String
          (Printf.sprintf "=> %s [ %s ]%!" directive
             (String.concat " " arguments))

let eval ast runtime =
  match ast with
    | Ast ast ->
        let runtime, r = 
          ClovisInterpreter.eval runtime ast 
        in
          (runtime, String (String.concat "\n" r))
    | s -> (runtime, s)

let print runtime s =
  match s with
    | String s -> s
    | Ast (_, e) -> Misc.FormatUtils.as_string ClovisPrinter.expression e

let interaction_system = Languages.mk_language_is parse_string print eval

let translate_ast = function
  | Hamlet.Ast ds ->
    let type_declaration denv = function
	| HamletAST.DVar (x, e) -> 
	  HamletTyper.typecheck denv (x, e)
	| HamletAST.DType _ -> ()
    in
    let rec translate_declaration = function
    | [] -> failwith "no content"
    | HamletAST.DVar (x, e) :: next -> 
        Ast (ClovisIdentifier.fresh (), ClovisFromHamlet.translate e)
    | (HamletAST.DType _)::next  -> translate_declaration next
    in
    let dataenv = HamletTyper.get_types ds in
    List.iter (type_declaration dataenv) ds;
    translate_declaration ds
  | _ -> assert false

let init () =
    Languages.declare_language name interaction_system;
    Languages.declare_translation 
      Hamlet.name name 
      Hamlet.interaction_system interaction_system 
      translate_ast
