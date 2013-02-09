open Willow

let print_flag    = ref 0
let notrace_flag  = ref 0
let compile_flag  = ref 0
let samename_flag = ref 0

let decalage_input = ref 2

let rec fun_h      = ref ""

let print_ast s =
  match s with
    | String s -> s
    | Ast (WillowInterpreter.Expression e) -> 
        Misc.FormatUtils.as_string CPrinter.preambule e
    | Ast (WillowInterpreter.Program p) -> 
	fun_h := CPrinter.create_h p;
        Misc.FormatUtils.as_string CPrinter.program p
    | Ast (WillowInterpreter.Declaration e) -> 
        Misc.FormatUtils.as_string CPrinter.function_declaration e

let print_help () = 
  let separator = ("=========================================\n  ") in
  let indent    = ("\n  ") in
  print_string indent;
  print_string ("Translate a Willow file to a C file"^indent);
  print_string separator;
  print_string ("[Syntax] ./toC {options}* Inputfile Outputfile"^indent);
  print_string (separator);
  print_string ("[Options list]"^indent);
  print_string ("-print    : Prints the C translation to the standard output (note : the C file is still written)"^indent);
  print_string ("-compile  : Compiles the translated file"^indent);
  print_string ("{ Exclusive options }"^indent);
  print_string ("  -notrace  : Does all the work in a temporary file instead of a permanent one."^indent);
  print_string ("  -samename : The outputfile is given the same name as the input one with the c extension inside the ./translated folder."^indent);
  print_string separator;
  print_string  ("\n");
  exit 0

let check_args () = 
  for i = 1 to Array.length Sys.argv - 1 do
	if Sys.argv.(i) = "-print"     then print_flag    := 1 else ();
	if Sys.argv.(i) = "-notrace"   then notrace_flag  := 1 else ();
	if Sys.argv.(i) = "-compile"   then compile_flag  := 1 else ();
	if Sys.argv.(i) = "-samename"  then samename_flag := 1 else ();
  done;
  if (!notrace_flag = 1 && !samename_flag = 1) then print_help () ;
  if (!notrace_flag = 1 || !samename_flag = 1) then decalage_input := 1 else () ;
  if Array.length Sys.argv < (!decalage_input + 2) then print_help () 

let get_output inputfile =
  if !notrace_flag = 1 then (Filename.temp_file "ted_translate" ".c")
  else if !samename_flag = 1 then ("./translated/"^(Filename.chop_extension (Filename.basename inputfile))^".c")
  else (Sys.argv.((Array.length Sys.argv) - 1))

let get_compiled_file output_file = 
  if !notrace_flag = 0 then ((Filename.chop_extension output_file)^".out")
  else Filename.temp_file "ted_cTemp" ".out"

let parsing filename =
    let e = 
          SyntacticAnalysis.process
            ~lexer_init: Misc.LexingUtils.from_filename
            ~lexer_fun: WillowLexer.main
            ~parser_fun: WillowParser.compilation_unit
            ~input: filename
    in
          Ast (WillowInterpreter.Program e)

let constants = "typedef void** (*fp)(void**,void**);\n\n"

let write_file buffer filename = 
  let fd = Unix.openfile filename [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o640 in
  let buffer = (constants^(!fun_h)^buffer) in
  let _ = Unix.single_write fd buffer 0 (String.length buffer) in
  Unix.close fd

let _ = 
  check_args ();
  let input_file =  Sys.argv.((Array.length Sys.argv) - (!decalage_input)) in
  let output_file = get_output input_file in
  let ast = parsing input_file in
  let translated_string = print_ast ast in
  if (!print_flag = 1) then begin
    print_string "\n";
    print_string translated_string; 
    print_string "\n\n" end
  else ();
  write_file translated_string output_file;
  if (!compile_flag = 1) then begin
    let compiled_file = get_compiled_file output_file in
    let _ = Sys.command ("gcc -O2 -w -o "^compiled_file^" "^output_file) in ()
  end else () 

