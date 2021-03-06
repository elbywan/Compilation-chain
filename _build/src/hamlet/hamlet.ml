open HamletAST

let name = "hamlet"

type input =
  | Ast    of declaration list
  | String of string

let parse_file filename =
  SyntacticAnalysis.process
    ~lexer_init: (fun filename -> Lexing.from_channel (open_in filename))
    ~lexer_fun: HamletLexer.main
    ~parser_fun: HamletParser.toplevel_expression
    ~input: filename

let dispatch_between_expression_and_declaration_regexp =
  Str.regexp "[' ' '\009' '\012' '\n']*val"

let is_declaration str =
  Str.string_match dispatch_between_expression_and_declaration_regexp str 0

let parse_string str =
  match Languages.parse_interactive_loop_directive str with
    | None ->
        if is_declaration str then
          Ast ([SyntacticAnalysis.process
                 ~lexer_init: Lexing.from_string
                 ~lexer_fun: HamletLexer.main
                 ~parser_fun: HamletParser.toplevel_declaration
                 ~input: str])
        else
          let ds, e = SyntacticAnalysis.process
            ~lexer_init: Lexing.from_string
            ~lexer_fun: HamletLexer.main
            ~parser_fun: HamletParser.toplevel_expression
            ~input: str
          in
            Ast (ds @ [DVar (HamletIdentifier.fresh (), e)])

    | Some ("load", [filename]) ->
        let ds, e = 
          SyntacticAnalysis.process
            ~lexer_init: Misc.LexingUtils.from_filename
            ~lexer_fun: HamletLexer.main
            ~parser_fun: HamletParser.toplevel_expression
            ~input: filename
        in
          Ast (ds @ [DVar (HamletIdentifier.fresh (), e)])

    | Some (directive, arguments) ->
        String
          (Printf.sprintf "=> %s [ %s ]%!" directive
             (String.concat " " arguments))

let eval ast runtime =
  let type_declaration denv = function
    | DVar (x, e) -> 
        HamletTyper.typecheck denv (x, e)
    | DType _ -> ()
  in
  let eval_declaration (runtime, r) = function
    | DVar (x, e) -> 
      let runtime, r' = HamletInterpreter.eval runtime (x, e) in
      (runtime, r @ r')
    | DType _ -> 
      (runtime, r)
  in
  match ast with
    | Ast ds ->
      let dataenv = HamletTyper.get_types ds in
      List.iter (type_declaration dataenv) ds;
      let runtime, rs = List.fold_left eval_declaration (runtime, []) ds in
      (runtime, String (String.concat "\n" rs))
    | s -> 
      (runtime, s)

let print runtime s =
  match s with
    | String s -> s
    | Ast tc -> Misc.FormatUtils.as_string HamletPrinter.declarations tc

let interaction_system = Languages.mk_language_is parse_string print eval

let init () =
  Languages.declare_language name interaction_system
