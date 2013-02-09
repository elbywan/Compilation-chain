module ExistentialDescription =
  Misc.Types.MakeExistential
    (struct type 'a t = 'a RuntimeSystem.interaction_system end)

type t = ExistentialDescription.t

let languages : (string, t) Hashtbl.t = Hashtbl.create 42

let selected_language = ref ""

let mk_language_is read print eval = 
  {
    RuntimeSystem.read        = read;
    RuntimeSystem.eval        = eval;
    RuntimeSystem.print       = print
  }

let declare_language ?(as_default=false) language_name is =
  if as_default then selected_language := language_name;
  Hashtbl.add languages language_name (ExistentialDescription.pack is)

let find_language_is language_name = 
  (try Hashtbl.find languages language_name with Not_found -> assert false)

type translation = string -> string -> unit

let make_translation f source_is target_is in_filename out_filename =
  let ast = RuntimeSystem.read_file in_filename source_is in
    Printf.eprintf "%s\n" (RuntimeSystem.print_ast ast source_is);
  let out_ast = f ast in
    Printf.eprintf "%s\n" (RuntimeSystem.print_ast out_ast target_is);
    RuntimeSystem.write_file out_filename (f ast) target_is

let compose_translation : translation -> translation -> translation =
  let tmp_filename = Filename.temp_file "ted-" ".tmp" in
    fun f g in_filename out_filename -> 
      g in_filename tmp_filename;
      f tmp_filename out_filename

let translations : (string, (string * translation) list ref) Hashtbl.t =
  Hashtbl.create 42

let declare_translation source target source_is target_is translation =
  let translation = 
    make_translation translation source_is target_is
  in
  let l = try Hashtbl.find translations source
  with Not_found ->
    let r = ref [] in
      Hashtbl.add translations source r;
      r
  in
    l := (target, translation) :: !l

let find_translation source target =
  let marks = Hashtbl.create 13 in
  let rec find source =
    if Hashtbl.mem marks source then
      None
    else let m = try ! (Hashtbl.find translations source) with Not_found -> [] in
      try
        Some (List.assoc target m)
      with Not_found ->
        Hashtbl.add marks source ();
        Misc.ListUtils.first_success m
          (fun (il, t') -> match find il with
             | None -> None
             | Some t -> Some (compose_translation t t'))
  in
    match find source with
      | None -> Error.global_error "(during initialization)"
          (Printf.sprintf "I do not known how to translate from %s to %s.\n" 
             source target)
      | Some t -> t

let interactive_loop_for language_name =
  ExistentialDescription.use
    { ExistentialDescription.f = RuntimeSystem.interactive_loop }
    (find_language_is language_name)

let interpret print_flag language_name filename =
  let interpret is = RuntimeSystem.interpret print_flag filename is in
    ExistentialDescription.use
      { ExistentialDescription.f = interpret }
      (try Hashtbl.find languages language_name with Not_found -> assert false)

let compile source filename = 
  let target = match !Settings.target with "default" -> source | x -> x in
  let outfilename = 
    Filename.chop_extension (Filename.basename filename) 
    ^ ".compiled-" ^ target 
  in
  let translate = find_translation source target in
    translate filename outfilename

let options () =
  let languages = Hashtbl.fold (fun l _ accu -> l :: accu) languages [] in
    List.fold_left
    (fun opts l ->
       ((Printf.sprintf "--%s" l),
        Arg.Unit (fun () -> selected_language := l),
        (Printf.sprintf " Focus on language `%s'. " l)
       ) :: opts)
    [("--target", Arg.Symbol (languages, fun s -> Settings.target := s),
     " Compile to a particular language.")]
    languages

let get_selected_language () =
  if !selected_language = "" then
    Error.global_error "during analysis of executable's arguments"
      "Please select a programming language to focus on."
  else
    !selected_language

let directive_regexp =
  Str.regexp "[' ' '\009' '\012']*#\\([a-z]+\\)"

let parse_arguments s =
  List.filter (( <> ) "") (Str.split (Str.regexp "[' ' '\009' '\012']+") s)

let parse_interactive_loop_directive str =
  if Str.string_match directive_regexp str 0 then
    let end_pos = Str.match_end () in
    let directive = Str.matched_group 1 str in
      Some (directive,
            (parse_arguments
               (String.sub str end_pos (String.length str - end_pos))))
  else
    None
