exception Error

type token = 
  | WITH
  | VAL
  | UID of (string)
  | TYPE
  | TYINT
  | TYFLOAT
  | TYARRAY
  | THEN
  | STARDOT
  | STAR
  | SLASHDOT
  | SLASH
  | SARROW
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | PROJ
  | PLUSDOT
  | PLUS
  | PIPE
  | OF
  | NEW
  | MINUSDOT
  | MINUS
  | MATCH
  | LTDOT
  | LT
  | LPAREN
  | LET
  | LBRACKET
  | LBRACE
  | INT of (int)
  | IN
  | IFZ
  | ID of (string)
  | FUN
  | FLOAT of (float)
  | FIX
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | COMMA
  | COLON
  | ASSIGN
  | ARROW
  | AND

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState164
  | MenhirState159
  | MenhirState155
  | MenhirState150
  | MenhirState147
  | MenhirState143
  | MenhirState142
  | MenhirState140
  | MenhirState137
  | MenhirState132
  | MenhirState126
  | MenhirState123
  | MenhirState118
  | MenhirState116
  | MenhirState115
  | MenhirState113
  | MenhirState108
  | MenhirState106
  | MenhirState103
  | MenhirState101
  | MenhirState97
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState75
  | MenhirState70
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState62
  | MenhirState59
  | MenhirState57
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState46
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState38
  | MenhirState36
  | MenhirState27
  | MenhirState24
  | MenhirState22
  | MenhirState21
  | MenhirState18
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState13
  | MenhirState12
  | MenhirState5
  | MenhirState2
  | MenhirState0

  
  open HamletAST
  open Position

  let parse_error = Error.error "during parsing"

let _eRR =
  Error

let rec _menhir_goto_nonempty_list_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (HamletAST.declaration list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_declaration_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_AND_separated_pair_binder_EQ_expression2__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((HamletAST.binder * HamletAST.expression) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, x0), _, y0), _, xs) = _menhir_stack in
        let _v : ((HamletAST.binder * HamletAST.expression) list) = let x =
          let y = y0 in
          let x = x0 in
              ( (x, y) )
        in
            ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_AND_separated_pair_binder_EQ_expression2__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_PIPE_constructor_declaration__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.constructor_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let (((_menhir_stack, _menhir_s), _, x), _, _) = _menhir_stack in
    let _v : (HamletAST.declaration) = let ds =
      let xs = xs0 in
          ( xs )
    in
                                                                                  (
  DType (x, ds)
) in
    _menhir_goto_declaration _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_PIPE_branch__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.clause list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, s), _, _), _, xs0) = _menhir_stack in
        let _v : (HamletAST.expression) = let bs =
          let xs = xs0 in
              ( xs )
        in
        (
  Match (s, bs)
) in
        _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.declaration) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, d) = _menhir_stack in
            let _v : (HamletAST.declaration) = (
  d
) in
            _menhir_goto_toplevel_declaration _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState164 | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TYPE ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | VAL ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (HamletAST.declaration list) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_declaration_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_PIPE_branch_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.clause list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (HamletAST.clause list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_PIPE_branch__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (HamletAST.clause list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_PIPE_branch_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState13 | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (HamletAST.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (HamletAST.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce30 : _menhir_env -> ('ttv_tail * _menhir_state * (HamletAST.expression)) * _menhir_state * (HamletAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, e), _, idx) = _menhir_stack in
    let _v : (HamletAST.expression) = (
  ArrayRead (e, idx)
) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, x), _, e) = _menhir_stack in
        let _v : (HamletAST.expression) = (
  Fix (x, e)
) in
        _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState155 | MenhirState159 | MenhirState5 | MenhirState132 | MenhirState116 | MenhirState12 | MenhirState13 | MenhirState75 | MenhirState70 | MenhirState62 | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | TYARRAY ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | TYFLOAT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | TYINT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | ARROW | COMMA | END | EOF | IN | PIPE | RBRACE | RBRACKET | RPAREN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _v : (HamletAST.expression) =                           (
  e
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, x), _, e) = _menhir_stack in
        let _v : (HamletAST.expression) = (
  Fun (x, e)
) in
        _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, cond), _), _, lhs), _, rhs) = _menhir_stack in
        let _v : (HamletAST.expression) = (
  App (Prim IfZ, 
       Tuple [ cond; 
               Fun ((HamletIdentifier.fresh (), None), lhs); 
               Fun ((HamletIdentifier.fresh (), None), rhs) ])
) in
        _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _, fs), _, rhs) = _menhir_stack in
        let _v : (HamletAST.expression) = (
  let (xs, es) = List.split fs in
    LetRec (xs, es, rhs)
) in
        _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, y0) = _menhir_stack in
            let _v : ((HamletAST.binder * HamletAST.expression) list) = let x =
              let y = y0 in
              let x = x0 in
                  ( (x, y) )
            in
                ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_AND_separated_pair_binder_EQ_expression2__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, x), _, lhs), _, rhs) = _menhir_stack in
        let _v : (HamletAST.expression) = (
  ( match x with | (x,None)       ->  App (Fun ((x,None), rhs), lhs)
	         | (x,Some thing) ->  App (Fun ((x,None), rhs), Annot (lhs, thing)) )
) in
        _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_goto_separated_nonempty_list_PIPE_constructor_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.constructor_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (HamletAST.constructor_declaration list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_PIPE_constructor_declaration__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (HamletAST.constructor_declaration list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_PIPE_constructor_declaration_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_STAR_ty__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, xs0) = _menhir_stack in
            let _v : (HamletAST.ty) = let ts =
              let xs = xs0 in
                  ( xs )
            in
            (
  match ts with
    | [t] -> t
    | ts -> TyTuple ts
) in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, d), _, xs0) = _menhir_stack in
        let _v : (HamletAST.constructor_declaration) = let tys =
          let xs = xs0 in
              ( xs )
        in
                                                             (
  (d, tys)
) in
        _menhir_goto_constructor_declaration _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, d), _, xs0) = _menhir_stack in
            let _v : (HamletAST.expression) = let es =
              let xs = xs0 in
                  ( xs )
            in
            (
  Data (d, es)
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, xs0) = _menhir_stack in
            let _v : (HamletAST.expression) = let es =
              let xs = xs0 in
                  ( xs )
            in
            (
  match es with
    | [e] -> e
    | es -> Tuple es
) in
            _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_PIPE_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | LPAREN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState115 in
            let _v : (HamletAST.clause list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_PIPE_branch__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | EOF | IN | TYPE | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState143 in
            let _v : (HamletAST.constructor_declaration list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_PIPE_constructor_declaration__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_pattern_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.pattern list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (HamletAST.pattern list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_pattern__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (HamletAST.pattern list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_pattern_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FLOAT _v ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
                | ID _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
                | INT _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
                | LPAREN ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | NEW ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | PROJ ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                | UID _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
            | AND | ARROW | COLON | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PIPE | PLUS | PLUSDOT | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | TYPE | UID _ | VAL | WITH ->
                _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 | MenhirState75 | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | RBRACE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (HamletAST.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | PIPE ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | END | ID _ | LPAREN | UID _ ->
                _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | LPAREN ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, p), _, e) = _menhir_stack in
        let _v : (HamletAST.clause) = (
  (p, e)
) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PIPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
            | LPAREN ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (HamletAST.clause list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_PIPE_branch_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, x), _, e) = _menhir_stack in
        let _v : (HamletAST.declaration) =                                               (
  DVar (x, e)
) in
        _menhir_goto_declaration _menhir_env _menhir_stack _menhir_s _v
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, ds), _, e) = _menhir_stack in
            let _v : (HamletAST.declaration list * HamletAST.expression) = (
  (ds, e)
) in
            _menhir_goto_toplevel_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _v : (HamletAST.declaration list * HamletAST.expression) = (
  ([], e)
) in
            _menhir_goto_toplevel_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_STAR_ty_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (HamletAST.ty list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_STAR_ty_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState147 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (HamletAST.ty list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_STAR_ty__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TYARRAY ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TYFLOAT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TYINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_reduce32 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, e) = _menhir_stack in
    let _v : (HamletAST.expression) = (
  e
) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LTDOT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | MINUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | MINUSDOT ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PLUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PLUSDOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SLASH ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SLASHDOT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | STAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | STARDOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState41 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState155 | MenhirState159 | MenhirState5 | MenhirState132 | MenhirState116 | MenhirState12 | MenhirState13 | MenhirState106 | MenhirState108 | MenhirState101 | MenhirState36 | MenhirState42 | MenhirState97 | MenhirState46 | MenhirState75 | MenhirState70 | MenhirState62 | MenhirState57 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LTDOT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MINUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MINUSDOT ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PLUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PLUSDOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SLASH ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SLASHDOT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | STAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | STARDOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | PIPE | RBRACE | RBRACKET | RPAREN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _v : (HamletAST.expression) = (
  e
) in
            _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | LT | LTDOT | MINUS | MINUSDOT | PIPE | PLUS | PLUSDOT | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | THEN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (HamletAST.expression) = let op =
                         ( MulFloat )
            in
            (
  App (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LTDOT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUSDOT ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUSDOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SLASH ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SLASHDOT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | STAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | STARDOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | PIPE | RBRACE | RBRACKET | RPAREN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e), _, idx), _, v) = _menhir_stack in
            let _v : (HamletAST.expression) = (
  ArrayWrite (e, idx, v)
) in
            _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | LT | LTDOT | MINUS | MINUSDOT | PIPE | PLUS | PLUSDOT | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | THEN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (HamletAST.expression) = let op =
                         ( Mul )
            in
            (
  App (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | LT | LTDOT | MINUS | MINUSDOT | PIPE | PLUS | PLUSDOT | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | THEN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (HamletAST.expression) = let op =
                         ( DivFloat )
            in
            (
  App (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | LT | LTDOT | MINUS | MINUSDOT | PIPE | PLUS | PLUSDOT | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | THEN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (HamletAST.expression) = let op =
                         ( Div )
            in
            (
  App (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SLASH ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SLASHDOT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | STAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | STARDOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | LT | LTDOT | MINUS | MINUSDOT | PIPE | PLUS | PLUSDOT | RBRACE | RBRACKET | RPAREN | THEN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (HamletAST.expression) = let op =
                         ( AddFloat )
            in
            (
  App (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SLASH ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SLASHDOT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | STAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | STARDOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | LT | LTDOT | MINUS | MINUSDOT | PIPE | PLUS | PLUSDOT | RBRACE | RBRACKET | RPAREN | THEN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (HamletAST.expression) = let op =
                         ( Add )
            in
            (
  App (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | SLASH ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | SLASHDOT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | STAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | STARDOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | LT | LTDOT | MINUS | MINUSDOT | PIPE | PLUS | PLUSDOT | RBRACE | RBRACKET | RPAREN | THEN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (HamletAST.expression) = let op =
                         ( SubFloat )
            in
            (
  App (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SLASH ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SLASHDOT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | STAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | STARDOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | LT | LTDOT | MINUS | MINUSDOT | PIPE | PLUS | PLUSDOT | RBRACE | RBRACKET | RPAREN | THEN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (HamletAST.expression) = let op =
                         ( Sub )
            in
            (
  App (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUSDOT ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUSDOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SLASH ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SLASHDOT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | STAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | STARDOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | PIPE | RBRACE | RBRACKET | RPAREN | THEN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (HamletAST.expression) = let op =
                         ( LessThanFloat )
            in
            (
  App (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | INT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUSDOT ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUSDOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SLASH ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SLASHDOT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | STAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | STARDOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | IN | PIPE | RBRACE | RBRACKET | RPAREN | THEN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (HamletAST.expression) = let op =
                         ( LessThanInt )
            in
            (
  App (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | _ ->
        _menhir_fail ()

and _menhir_goto_toplevel_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.declaration) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Obj.magic _1

and _menhir_goto_toplevel_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.declaration list * HamletAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Obj.magic _1

and _menhir_goto_constructor_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.constructor_declaration) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PIPE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
    | EOF | IN | TYPE | VAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (HamletAST.constructor_declaration list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_PIPE_constructor_declaration_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (HamletAST.ty list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_STAR_ty__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_pattern__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.pattern list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs0) = _menhir_stack in
        let _v : (HamletAST.pattern) = let ps =
          let xs = xs0 in
              ( xs )
        in
        (
  PData (x, ps)
) in
        _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run116 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FIX ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | FUN ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | IFZ ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | LET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_reduce15 : _menhir_env -> 'ttv_tail * _menhir_state * (HamletAST.data_constructor) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, d) = _menhir_stack in
    let _v : (HamletAST.expression) = (
  Data (d, [])
) in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (HamletAST.expression list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_PIPE_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run114 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_PIPE_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_pattern : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.pattern) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, e), _, p) = _menhir_stack in
            let _v : (HamletAST.pattern) = (
  PView (e, p)
) in
            _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | LPAREN ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (HamletAST.pattern list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_pattern_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState137 | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_binder : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.binder) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState147 | MenhirState24 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SARROW ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | TYARRAY ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | TYFLOAT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | TYINT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
        | EOF | IN | PIPE | RPAREN | TYPE | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (HamletAST.ty list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_STAR_ty_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SARROW ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | ARROW | COMMA | END | EOF | IN | PIPE | RBRACE | RBRACKET | RPAREN | STAR | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (HamletAST.ty) = (
  TyArrow (lhs, rhs, Var (HamletIdentifier.fresh ()), HamletIdentifier.fresh (), Identifier.IMap.empty)
) in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, ty) = _menhir_stack in
        let _v : (HamletAST.ty) = (
  TyArray ty
) in
        _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, x), _, ty) = _menhir_stack in
            let _v : (HamletAST.binder) =                                          (
  (x, Some ty)
) in
            _menhir_goto_binder _menhir_env _menhir_stack _menhir_s _v
        | SARROW ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SARROW ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | ARROW | COMMA | END | EOF | IN | PIPE | RBRACE | RBRACKET | RPAREN | TYPE | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, ty) = _menhir_stack in
            let _v : (HamletAST.expression) =                             (
  Annot (e, ty)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (HamletAST.ty) = (
  TyInt
) in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (HamletAST.ty) = (
  TyFloat
) in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | TYARRAY ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | TYFLOAT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | TYINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TYARRAY ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TYFLOAT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TYINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | RPAREN ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_goto_expression0 : _menhir_env -> 'ttv_tail -> _menhir_state -> (HamletAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 | MenhirState51 | MenhirState66 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState68 | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
        let _v : (HamletAST.expression) = (
  App (e1, e2)
) in
        _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState38 | MenhirState65 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState67 | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PIPE | PLUS | PLUSDOT | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | THEN | TYPE | UID _ | VAL | WITH ->
            _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 | MenhirState159 | MenhirState5 | MenhirState132 | MenhirState116 | MenhirState12 | MenhirState13 | MenhirState106 | MenhirState108 | MenhirState101 | MenhirState36 | MenhirState42 | MenhirState97 | MenhirState46 | MenhirState49 | MenhirState75 | MenhirState70 | MenhirState62 | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PIPE | PLUS | PLUSDOT | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | TYPE | UID _ | VAL | WITH ->
            _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState155 in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__1_ = _startpos in
        let _endpos__1_ = _endpos in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (HamletAST.declaration list * HamletAST.expression) = (
  parse_error (Position.lex_join _startpos _endpos) "Syntax error."
) in
        _menhir_goto_toplevel_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__1_ = _startpos in
        let _endpos__1_ = _endpos in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (HamletAST.declaration) = (
  parse_error (Position.lex_join _startpos _endpos) "Syntax error."
) in
        _menhir_goto_toplevel_declaration _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (HamletAST.data_constructor) = (
  Identifier.mk HamletIdentifier.data_constructor_kind x
) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 | MenhirState51 | MenhirState66 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState68 | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState155 | MenhirState159 | MenhirState5 | MenhirState12 | MenhirState132 | MenhirState116 | MenhirState13 | MenhirState106 | MenhirState108 | MenhirState101 | MenhirState36 | MenhirState38 | MenhirState42 | MenhirState97 | MenhirState46 | MenhirState49 | MenhirState52 | MenhirState57 | MenhirState62 | MenhirState65 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState70 | MenhirState75 | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | RBRACE ->
                _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
        | AND | ARROW | COLON | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LBRACKET | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PIPE | PLUS | PLUSDOT | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | THEN | TYPE | UID _ | VAL | WITH ->
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState137 | MenhirState115 | MenhirState123 | MenhirState126 | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | LPAREN ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState123 in
                let _v : (HamletAST.pattern list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_pattern__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | ARROW | COMMA | RBRACE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (HamletAST.pattern) = (
  PData (x, [])
) in
            _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState150 | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | TYARRAY ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | TYFLOAT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | TYINT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | EOF | IN | PIPE | TYPE | VAL ->
                _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
        | EOF | IN | PIPE | TYPE | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, d) = _menhir_stack in
            let _v : (HamletAST.constructor_declaration) = (
  (d, [])
) in
            _menhir_goto_constructor_declaration _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run140 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | INT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), x) = _menhir_stack in
                let _v : (HamletAST.expression) = (
  Prim (Proj x)
) in
                _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (HamletAST.expression) = (
  Prim AllocArray
) in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FIX ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | FUN ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | IFZ ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | LET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FIX ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | FUN ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IFZ ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | RPAREN ->
        _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | REC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState14 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (HamletAST.expression) = (
  Int x
) in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (HamletAST.tyvar) = (
  Identifier.mk HamletIdentifier.value_kind x
) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FIX ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | FLOAT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | FUN ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | IFZ ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | INT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | LET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | TYARRAY ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | TYFLOAT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | TYINT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState147 | MenhirState59 | MenhirState18 | MenhirState21 | MenhirState22 | MenhirState27 | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, d) = _menhir_stack in
        let _v : (HamletAST.ty) = (
  TyADT d
) in
        _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v
    | MenhirState155 | MenhirState159 | MenhirState5 | MenhirState132 | MenhirState116 | MenhirState12 | MenhirState13 | MenhirState108 | MenhirState106 | MenhirState101 | MenhirState36 | MenhirState41 | MenhirState97 | MenhirState42 | MenhirState46 | MenhirState51 | MenhirState66 | MenhirState92 | MenhirState91 | MenhirState90 | MenhirState89 | MenhirState88 | MenhirState87 | MenhirState86 | MenhirState85 | MenhirState84 | MenhirState83 | MenhirState82 | MenhirState81 | MenhirState80 | MenhirState79 | MenhirState78 | MenhirState77 | MenhirState75 | MenhirState70 | MenhirState68 | MenhirState67 | MenhirState65 | MenhirState62 | MenhirState57 | MenhirState53 | MenhirState52 | MenhirState49 | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (HamletAST.expression) = (
  Var x
) in
        _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState14 | MenhirState103 | MenhirState15 | MenhirState47 | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (HamletAST.binder) =                      (
  (x, None)
) in
        _menhir_goto_binder _menhir_env _menhir_stack _menhir_s _v
    | MenhirState137 | MenhirState115 | MenhirState123 | MenhirState126 | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (HamletAST.pattern) = (
  PVar x
) in
        _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | PIPE ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | EOF | IN | TYPE | UID _ | VAL ->
                _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (HamletAST.expression) = (
  Float x
) in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = 4611686018427387903;
      }

and toplevel_declaration : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (HamletAST.declaration) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TYPE ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and toplevel_expression : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (HamletAST.declaration list * HamletAST.expression) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FIX ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | FLOAT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | FUN ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | IFZ ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | INT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | LET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | TYPE ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | VAL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)



