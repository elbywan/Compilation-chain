{
 open WillowParser
 open Lexing
 open ExtLexing
 open Error

 (** Keywords *)
 let keywords = [
   "let",         LET;
   "fun",         FUN;
   "in",          IN;
   "ifz",         IFZ;
   "then",        THEN;
   "else",        ELSE;
   (** UPDATE ++ **)
   "new",	  NEW;
 ]

 let is_keyword =
   let h = Hashtbl.create 13 in
     List.iter (fun (s, tok) -> Hashtbl.add h s tok) keywords;
     Hashtbl.find h

}

(*-------------------*
 | Lexical classes.  |
 *-------------------*)

(** Layout. *)

let newline = ('\010' | '\013' | "\013\010")

let blank = [' ' '\009' '\012']

(** Alphanumeric characters classes. *)

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'
   '\'' '?' ]

let integer = [ '0'-'9' ]

let hexa = [ '0'-'9' 'a'-'f' 'A'-'F' ]

(** Identifiers. *)

let identifier  = identchar+

let fidentifier = "`" identchar+

(*------------------*
 | Analysis rules.  |
 *------------------*)

rule main = parse

(** Layout. *)

| newline                               { next_line_and main lexbuf }
| blank+                                { main lexbuf }
| eof                                   { EOF }

(** Punctuation. *)

| '='                                   { EQ }
| ','                                   { COMMA }
| '('                                   { LPAREN }
| ')'                                   { RPAREN }
| '['                                   { LBRACKET }
| ']'                                   { RBRACKET }
| '+'                                   { PLUS }
| '-'                                   { MINUS }
| '*'                                   { STAR }
| '/'                                   { SLASH }
(** UPDATE ++ **)
| "+."                                  { PLUSDOT }
| "-."                                  { MINUSDOT }
| "*."                                  { STARDOT }
| "/."                                  { SLASHDOT }
| "<."                                  { LTDOT }
| "<"                                   { LT }

(** Symbol. *)
| "<-"                                  { LEFTARROW }

(** Constants. *)
| integer+                              { INT (int_of_string (lexeme lexbuf)) }
(** UPDATE ++ **)
| integer+ '.' integer*                 { FLOAT (float_of_string (lexeme lexbuf)) }

(** Identifiers and keywords. *)

| fidentifier                           { FID (lexeme lexbuf) }

| identifier                            { let id = lexeme lexbuf in
                                            try is_keyword id with
                                                Not_found ->
                                                  ID (id)
                                        }

(** Comments *)

| "(*"                                  { comment 0 lexbuf }

(** Lexical error handling. *)

| _                                     { lexical_error lexbuf "." }

(*---------------------------*
 | Comments analysis rules.  |
 *---------------------------*)

and comment level = parse

(** Decrease the nesting level. *)

| "*)"                                  {
    if level = 0 then main lexbuf else comment (level - 1) lexbuf
}

(** Increase the nesting level. *)

| "(*"                                  {
    comment (level + 1) lexbuf
}

(** A comment must be closed before the end of the file. *)

| eof                                   {
    lexical_error lexbuf "Unterminated comment"
}

(** Layout. *)

| newline                               {
    next_line_and (comment level) lexbuf
}

(** Everything else is absorbed by the comment. *)

| _                                     {
    comment level lexbuf
}
