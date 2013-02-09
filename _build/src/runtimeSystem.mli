(**

    FIXME: To be completed by the definition of the lecture notes.

    From 
    {{:http://en.wikipedia.org/wiki/Run-time_system}
    Wikipedia}:

    A run-time system (also called runtime system or just runtime) is a
    collection of software designed to support the execution of computer
    programs written in some computer language. The run-time system may
    provide software services such as subroutines and libraries for common
    operations, implementation of programming language commands, type
    checking, debugging, and eve code generation and code optimization.

    Some services of the run-time system are often accessible to the
    programmer through an application programming interface; but some
    services (such as task scheduling and resource management) may be
    inaccessible.

    A run-time system relieves programmers from the burden of writing
    code for mundane tasks such as drawing text on the screen or
    making an Internet connection. It also provides an abstraction
    layer that hides the complexity or variations in the services
    offered by the operating system. In the limit, the run-time system
    may be a P-code machine or virtual machine, that hides even the
    processor's instruction set. This is the approach followed by many
    interpreted languages such as Lisp and Awk, and some languages
    like Java that are meant to be compiled into some
    machine-independent pseudo-code ("bytecode"). This arrangement
    greatly simplifies the task of language implementation and its
    adaptation to different machines, and allows sophisticated
    language features such as reflection (computing). It also allows
    the same program to be executed on any machine without
    recompilation, a feature that has become very important since
    after the diffusion of the World Wide Web.
*)
type t

(** A read-eval-print system.

    Internal values of type ['a] are abstract syntax trees (or any
    equivalent data structure) specific to the programming language
    we focus on.

    FIXME: Document invariants and properties.
           (for instance, can the result of the printer be used as input of
           the read function ?)

    FIXME: Some runtime system directives will probably be added using
           a wrapper to read and eval. An exception will be used not to
           pollute the type ['a]. 

    From 
    {{:http://en.wikipedia.org/wiki/Read-eval-print_loop}
    Wikipedia}:

    A read-eval-print loop (REPL), also known as an interactive toplevel,
    is a simple, interactive computer programming environment. The term is
    most usually used to refer to a Lisp interactive environment, but can
    be applied to command line shells and similar environments for
    Smalltalk, Perl, Python, Ruby, Haskell, APL, BASIC, J, Scheme, TCL,
    and other languages as well.

    In a REPL, the user may enter expressions, which are then
    evaluated, and the results displayed. The name read-eval-print
    loop comes from the names of the Lisp primitive functions which
    implement this functionality:

    The read function accepts a single expression from the user, and
    parses it into a data structure in memory. For instance, the user
    may enter the s-expression (+ 1 2 3), which is parsed into a
    linked list containing four data elements.  The eval function
    takes this internal data structure and evaluates it. In Lisp,
    evaluating an s-expression beginning with the name of a function
    means calling that function on the arguments that make up the rest
    of the expression. So the function + is called on the arguments 1
    2 3, yielding the result 6.  The print function takes the result
    yielded by eval, and prints it out to the user. If it is a complex
    expression, it may be pretty-printed to make it easier to
    understand. In this example, though, the number 6 does not need
    much formatting to print.

    The REPL is commonly misnamed an interpreter. This is a
    misnomer-many programming languages that use compilation
    (including bytecode compilation) have REPLs, such as Common Lisp
    and Python.

    Because the print function outputs in the same textual format that
    the read function uses for input, most results are printed in a
    form that could (if it's useful) be copied and pasted back into
    the REPL. However, it's sometimes necessary to print
    representations of opaque data elements that can't sensibly be
    read back in, such as a socket handle or a complex class
    instance. In these cases, there must exist a syntax for unreadable
    objects. In Python, it's the <__module__.class instance> notation,
    and in Common Lisp, the #<whatever> form. The REPL of CLIM, SLIME,
    and the Symbolics Lisp Machine can also read back unreadable
    objects. They record for each output which object was
    printed. Later when the code is read back, the object will be
    retrieved from the printed output.

*)
type 'a interaction_system = {
  read  : string -> 'a;
  eval  : 'a -> t -> t * 'a;
  print : t -> 'a -> string
}

(** Launch an interactive loop. *)
val interactive_loop : 'a interaction_system -> unit

(** Interpret a whole file. *)
val interpret : bool -> string -> 'a interaction_system -> unit

(** Simply read an input file. *)
val read_file : string -> 'a interaction_system -> 'a

(** Simply write an output file by printing an AST. *)
val write_file : string -> 'a -> 'a interaction_system -> unit

(** Print an AST. *)
val print_ast : 'a -> 'a interaction_system -> string
