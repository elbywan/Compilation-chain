let (_ : unit) =
  Hamlet.init ();
  Clovis.init ();
  Willow.init ()

let options = Arg.align
  ([
     "-i",            Arg.Set Settings.interactive, " ";
     "--interactive", Arg.Set Settings.interactive, 
     " Interactive mode.";
     "-e",            Arg.Set Settings.interpret,   " ";
     "--interpret", Arg.Set Settings.interpret, 
     " Interpret the input file.";
     "-p",            Arg.Set Settings.print,   " ";
     "--print", Arg.Set Settings.print, 
     " Pretty-print the input file (without syntactic sugar).";
     "-c",            Arg.Set Settings.compile, " ";
     "--compile", Arg.Set Settings.compile, 
     " Compile the input file.";
     "-d",            Arg.Set Settings.debug, " ";
     "--debug", Arg.Set Settings.debug, 
     " Set debug mode.";
   ]
   @ (Languages.options ()))

let usage_msg =
  Printf.sprintf "%s:" (Filename.basename Sys.executable_name)

let filenames =
  let filenames = ref [] in
    Arg.parse options (fun f -> filenames := f :: !filenames) usage_msg;
    !filenames

let _ =
  if !Settings.interactive then begin
    Languages.interactive_loop_for (Languages.get_selected_language ())
  end
  else if !Settings.interpret then begin
    List.iter 
      (Languages.interpret !Settings.print 
         (Languages.get_selected_language ())) 
      filenames
  end else if !Settings.compile then begin
    List.iter 
      (Languages.compile (Languages.get_selected_language ())) 
      filenames
  end
