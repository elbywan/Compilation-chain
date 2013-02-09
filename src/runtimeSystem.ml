type t = unit

let initial_runtime () =
  ()

type 'a exit_value =
  | Error  of Position.t list * string
  | Result of 'a

type 'a interaction_system = {
  read  : string -> 'a;
  eval  : 'a -> t -> t * 'a;
  print : t -> 'a -> string
}

let print_exit_value runtime = function
  | Error (pos, msg) ->
      output_string stdout (Error.print_error pos msg)
  | Result v ->
      output_string stdout v

let handle_error runtime (f : t -> t * 'a) =
  try
    let runtime, v = f runtime in
      (runtime, Result v)
  with Error.Error (pos, msg) ->
    (runtime, Error (pos, msg))

let read runtime input is = 
  snd (handle_error runtime (fun rt -> rt, is.read input))

let read_eval print_value runtime is input = 
  let runtime, exit_value =
    match read runtime input is with
      | Result ast -> handle_error runtime 
          (fun r -> 
             let (r, s) = is.eval ast r in 
               (r, is.print r ast ^ "\n" ^ is.print r s))
      | (Error _ as t) -> runtime, t
  in
    print_exit_value runtime exit_value;
    output_string stdout "\n";
    flush stdout;
    runtime

let interactive_loop is =
  let readline prompt =
    UserInput.set_prompt prompt;
    UserInput.set_ascii ();
    let buf = Buffer.create 256 in
    let rec loop c = 
      (* ASCII mode ensures that length of c is equal to 1. *)
      match c.[0] with
      | '\n' -> Buffer.contents buf
      | _    -> Buffer.add_string buf c; loop (UserInput.input_char stdin)
    in
      loop (UserInput.input_char stdin)
  in
  let rec interact runtime =
    interact (read_eval false runtime is (readline "> "))
  in
    Error.resume_if_error ();
    interact (initial_runtime ())

let interpret print_flag filename is = 
  Error.resume_if_error ();
  read_eval print_flag 
    (initial_runtime ()) is (Printf.sprintf "#load %s" filename)
    
let read_file filename is = 
  let runtime = initial_runtime () in
  match read runtime (Printf.sprintf "#load %s" filename) is with
    | Result ast -> ast
    | Error (pos, msg) -> 
        Error.error "(during parsing)" (List.hd pos) msg

let print_ast ast is = 
  is.print (initial_runtime ()) ast  

let write_file filename ast is =
  let runtime = initial_runtime () in
  Misc.IOUtils.write_contents filename (is.print runtime ast)
