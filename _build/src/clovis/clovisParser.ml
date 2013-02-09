exception Error

type token = 
  | WITH
  | VAL
  | UID of (string)
  | THEN
  | STARDOT
  | STAR
  | SLASHDOT
  | SLASH
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | PROJ
  | PLUSDOT
  | PLUS
  | PIPE
  | NEW
  | MINUSDOT
  | MINUS
  | MATCH
  | LTDOT
  | LT
  | LPAREN
  | LOOKUP
  | LET
  | LBRACKET
  | LBRACE
  | INT of (int)
  | IN
  | IFZ
  | ID of (string)
  | FLOAT of (float)
  | FIX
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | COMMA
  | CLOSURE
  | ASSIGN
  | ARROW
  | APPLY
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
  | MenhirState135
  | MenhirState129
  | MenhirState124
  | MenhirState118
  | MenhirState115
  | MenhirState110
  | MenhirState108
  | MenhirState107
  | MenhirState102
  | MenhirState96
  | MenhirState94
  | MenhirState91
  | MenhirState89
  | MenhirState85
  | MenhirState83
  | MenhirState82
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState53
  | MenhirState47
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState29
  | MenhirState26
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState13
  | MenhirState12
  | MenhirState5
  | MenhirState2
  | MenhirState0

  
  open ClovisAST
  open Position

  let parse_error = Error.error "during parsing"

let _eRR =
  Error

let rec _menhir_goto_loption_separated_nonempty_list_PIPE_branch__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.clause list) -> 'ttv_return =
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
        let ((((_menhir_stack, _menhir_s), _, s), _), _, xs0) = _menhir_stack in
        let _v : (ClovisAST.expression) = let bs =
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

and _menhir_goto_separated_nonempty_list_COMMA_expression2_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (ClovisAST.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression2__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (ClovisAST.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression2_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_AND_separated_pair_identifier_EQ_expression2__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((ClovisAST.var * ClovisAST.expression) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, x0), _, y0), _, xs) = _menhir_stack in
        let _v : ((ClovisAST.var * ClovisAST.expression) list) = let x =
          let y = y0 in
          let x = x0 in
              ( (x, y) )
        in
            ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_AND_separated_pair_identifier_EQ_expression2__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_PIPE_branch_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.clause list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (ClovisAST.clause list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_PIPE_branch__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (ClovisAST.clause list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_PIPE_branch_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_PIPE_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | LPAREN ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState107 in
        let _v : (ClovisAST.clause list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_PIPE_branch__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (ClovisAST.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (ClovisAST.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce25 : _menhir_env -> ('ttv_tail * _menhir_state * (ClovisAST.expression)) * _menhir_state * (ClovisAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, e), _, idx) = _menhir_stack in
    let _v : (ClovisAST.expression) = (
  ArrayRead (e, idx)
) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, ds), _, x), _, e) = _menhir_stack in
            let _v : (ClovisAST.expression) = (
  Closure (ds, x, e)
) in
            _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState135 | MenhirState5 | MenhirState124 | MenhirState108 | MenhirState12 | MenhirState29 | MenhirState73 | MenhirState53 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, e) = _menhir_stack in
        let _v : (ClovisAST.expression) =                           (
  e
) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState53 ->
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
                    | APPLY ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                    | FLOAT _v ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
                    | ID _v ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
                    | INT _v ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
                    | LOOKUP ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                    | LPAREN ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                    | NEW ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                    | PROJ ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                    | UID _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
                | AND | ARROW | COMMA | ELSE | END | EOF | IN | LT | LTDOT | MINUS | MINUSDOT | PIPE | PLUS | PLUSDOT | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | VAL | WITH ->
                    _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
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
        | MenhirState73 | MenhirState47 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | APPLY ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | CLOSURE ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | FIX ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | FLOAT _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                | ID _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                | IFZ ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | INT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                | LET ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | LOOKUP ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | LPAREN ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | MATCH ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | NEW ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | PROJ ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | UID _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : (ClovisAST.expression list) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState29 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
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
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _ = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let x = () in
                    let _v : (unit option) =     ( Some x ) in
                    _menhir_goto_option_PIPE_ _menhir_env _menhir_stack _v
                | END | ID _ | LPAREN | UID _ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _v : (unit option) =     ( None ) in
                    _menhir_goto_option_PIPE_ _menhir_env _menhir_stack _v
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
        | MenhirState108 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | ID _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
                | LPAREN ->
                    _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | UID _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState124 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, p), _, e) = _menhir_stack in
            let _v : (ClovisAST.clause) = (
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
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
                | LPAREN ->
                    _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | UID _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : (ClovisAST.clause list) =     ( [ x ] ) in
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
            let _v : (ClovisAST.declaration) =                                               (
  (x, e)
) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState75 | MenhirState34 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAL ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | PIPE ->
                    _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
            | MenhirState0 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EOF ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, d) = _menhir_stack in
                    let _v : (ClovisAST.declaration) = (
  d
) in
                    _menhir_goto_toplevel_declaration _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                _menhir_fail ())
        | MenhirState135 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, e) = _menhir_stack in
                let _v : (ClovisAST.expression) = (
  e
) in
                _menhir_goto_toplevel_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, x), _, e) = _menhir_stack in
        let _v : (ClovisAST.expression) = (
  Fix (x, e)
) in
        _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, cond), _), _, lhs), _, rhs) = _menhir_stack in
        let _v : (ClovisAST.expression) = (
  Apply (Prim IfZ, Tuple [ cond; lhs; rhs ])
) in
        _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _, fs), _, rhs) = _menhir_stack in
        let _v : (ClovisAST.expression) = (
  let (xs, es) = List.split fs in
    LetRec (xs, es, rhs)
) in
        _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, y0) = _menhir_stack in
            let _v : ((ClovisAST.var * ClovisAST.expression) list) = let x =
              let y = y0 in
              let x = x0 in
                  ( (x, y) )
            in
                ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_AND_separated_pair_identifier_EQ_expression2__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, x), _, lhs), _, rhs) = _menhir_stack in
        let _v : (ClovisAST.expression) = (
  Let ((x, lhs), rhs)
) in
        _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState102 | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (ClovisAST.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression2_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_goto_separated_nonempty_list_COMMA_pattern_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.pattern list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (ClovisAST.pattern list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_pattern__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (ClovisAST.pattern list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_pattern_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | INT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | LOOKUP ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | LTDOT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | MINUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | MINUSDOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | NEW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | PLUSDOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | PROJ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | STAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | UID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
        let _v : (ClovisAST.expression) = let op =
                     ( MulFloat )
        in
        (
  Apply (Prim op, Tuple [ lhs; rhs ])
) in
        _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState135 | MenhirState5 | MenhirState124 | MenhirState108 | MenhirState12 | MenhirState13 | MenhirState102 | MenhirState94 | MenhirState96 | MenhirState89 | MenhirState19 | MenhirState83 | MenhirState85 | MenhirState29 | MenhirState32 | MenhirState73 | MenhirState53 | MenhirState47 | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LTDOT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | MINUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | MINUSDOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PLUSDOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | STAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | AND | ARROW | COMMA | ELSE | END | EOF | IN | PIPE | RBRACE | RBRACKET | RPAREN | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _v : (ClovisAST.expression) = (
  e
) in
            _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LTDOT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | MINUSDOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PLUSDOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | AND | ARROW | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LOOKUP | LPAREN | MINUS | NEW | PIPE | PLUS | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | STAR | THEN | UID _ | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (ClovisAST.expression) = let op =
                         ( Mul )
            in
            (
  Apply (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
        let _v : (ClovisAST.expression) = let op =
                     ( DivFloat )
        in
        (
  Apply (Prim op, Tuple [ lhs; rhs ])
) in
        _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LTDOT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MINUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MINUSDOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PLUSDOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | STAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | AND | ARROW | COMMA | ELSE | END | EOF | IN | PIPE | RBRACE | RBRACKET | RPAREN | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e), _, idx), _, v) = _menhir_stack in
            let _v : (ClovisAST.expression) = (
  ArrayWrite (e, idx, v)
) in
            _menhir_goto_expression2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LTDOT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MINUSDOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | PLUSDOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | AND | ARROW | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LOOKUP | LPAREN | MINUS | NEW | PIPE | PLUS | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | STAR | THEN | UID _ | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (ClovisAST.expression) = let op =
                         ( Div )
            in
            (
  Apply (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | AND | ARROW | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LOOKUP | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PIPE | PLUS | PLUSDOT | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | STAR | THEN | UID _ | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (ClovisAST.expression) = let op =
                         ( AddFloat )
            in
            (
  Apply (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | AND | ARROW | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LOOKUP | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PIPE | PLUS | PLUSDOT | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | STAR | THEN | UID _ | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (ClovisAST.expression) = let op =
                         ( SubFloat )
            in
            (
  Apply (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUSDOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PLUSDOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | AND | ARROW | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LOOKUP | LPAREN | MINUS | NEW | PIPE | PLUS | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | STAR | THEN | UID _ | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (ClovisAST.expression) = let op =
                         ( LessThanFloat )
            in
            (
  Apply (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUSDOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUSDOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | AND | ARROW | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LOOKUP | LPAREN | MINUS | NEW | PIPE | PLUS | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | STAR | THEN | UID _ | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (ClovisAST.expression) = let op =
                         ( LessThanInt )
            in
            (
  Apply (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LTDOT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MINUSDOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUSDOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | STAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | AND | ARROW | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LOOKUP | LPAREN | MINUS | NEW | PIPE | PLUS | PROJ | RBRACE | RBRACKET | RPAREN | THEN | UID _ | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (ClovisAST.expression) = let op =
                         ( Add )
            in
            (
  Apply (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LTDOT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MINUSDOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUSDOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | STAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | AND | ARROW | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LOOKUP | LPAREN | MINUS | NEW | PIPE | PLUS | PROJ | RBRACE | RBRACKET | RPAREN | THEN | UID _ | VAL | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, lhs), _), _, rhs) = _menhir_stack in
            let _v : (ClovisAST.expression) = let op =
                         ( Sub )
            in
            (
  Apply (Prim op, Tuple [ lhs; rhs ])
) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LTDOT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUSDOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUSDOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SLASHDOT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | STAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | STARDOT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState82 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | _ ->
        _menhir_fail ()

and _menhir_reduce27 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, e) = _menhir_stack in
    let _v : (ClovisAST.expression) = (
  e
) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PIPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
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
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (ClovisAST.declaration list) =     ( x :: xs ) in
        _menhir_goto_list_declaration_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_toplevel_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.declaration) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Obj.magic _1

and _menhir_goto_toplevel_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Obj.magic _1

and _menhir_goto_loption_separated_nonempty_list_COMMA_pattern__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.pattern list) -> 'ttv_return =
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
        let _v : (ClovisAST.pattern) = let ps =
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

and _menhir_run108 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | CLOSURE ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | FIX ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | IFZ ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | LET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | LOOKUP ->
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_reduce10 : _menhir_env -> 'ttv_tail * _menhir_state * (ClovisAST.data_constructor) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, d) = _menhir_stack in
    let _v : (ClovisAST.expression) = (
  Data (d, [])
) in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.expression list) -> 'ttv_return =
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
        let ((_menhir_stack, _menhir_s, d), _, xs0) = _menhir_stack in
        let _v : (ClovisAST.expression) = let es =
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression2__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, xs0) = _menhir_stack in
        let _v : (ClovisAST.expression) = let es =
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_pattern : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.pattern) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, e), _, p) = _menhir_stack in
            let _v : (ClovisAST.pattern) = (
  PView (e, p)
) in
            _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | LPAREN ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (ClovisAST.pattern list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_pattern_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression0 : _menhir_env -> 'ttv_tail -> _menhir_state -> (ClovisAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 | MenhirState23 | MenhirState42 | MenhirState56 | MenhirState70 | MenhirState68 | MenhirState58 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState44 | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
        | AND | ARROW | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LOOKUP | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PIPE | PLUS | PLUSDOT | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | THEN | UID _ | VAL | WITH ->
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState135 | MenhirState5 | MenhirState124 | MenhirState108 | MenhirState12 | MenhirState13 | MenhirState102 | MenhirState94 | MenhirState96 | MenhirState89 | MenhirState19 | MenhirState83 | MenhirState85 | MenhirState29 | MenhirState32 | MenhirState38 | MenhirState73 | MenhirState53 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | AND | ARROW | COMMA | ELSE | END | EOF | IN | LT | LTDOT | MINUS | MINUSDOT | PIPE | PLUS | PLUSDOT | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | VAL | WITH ->
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, e1), _, e2) = _menhir_stack in
        let _v : (ClovisAST.expression) = (
  Apply (e1, e2)
) in
        _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (ClovisAST.declaration list) =     ( [] ) in
    _menhir_goto_list_declaration_ _menhir_env _menhir_stack _menhir_s _v

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

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState135 in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__1_ = _startpos in
        let _endpos__1_ = _endpos in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (ClovisAST.expression) = (
  parse_error (Position.lex_join _startpos _endpos) "Syntax error."
) in
        _menhir_goto_toplevel_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
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
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
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
        let _v : (ClovisAST.declaration) = (
  parse_error (Position.lex_join _startpos _endpos) "Syntax error."
) in
        _menhir_goto_toplevel_declaration _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (ClovisAST.data_constructor) = (
  Identifier.mk HamletIdentifier.data_constructor_kind x
) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState135 | MenhirState5 | MenhirState12 | MenhirState124 | MenhirState108 | MenhirState13 | MenhirState102 | MenhirState94 | MenhirState96 | MenhirState89 | MenhirState19 | MenhirState21 | MenhirState83 | MenhirState85 | MenhirState23 | MenhirState26 | MenhirState29 | MenhirState32 | MenhirState38 | MenhirState42 | MenhirState47 | MenhirState73 | MenhirState53 | MenhirState56 | MenhirState70 | MenhirState68 | MenhirState58 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState47 in
                let _v : (ClovisAST.expression list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
        | AND | ARROW | COMMA | ELSE | END | EOF | FLOAT _ | ID _ | IN | INT _ | LBRACKET | LOOKUP | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PIPE | PLUS | PLUSDOT | PROJ | RBRACE | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | THEN | UID _ | VAL | WITH ->
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState129 | MenhirState107 | MenhirState115 | MenhirState118 | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | LPAREN ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState115 in
                let _v : (ClovisAST.pattern list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_pattern__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
        | ARROW | COMMA | RBRACE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (ClovisAST.pattern) = (
  PData (x, [])
) in
            _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

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
                let _v : (ClovisAST.expression) = (
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
    let _v : (ClovisAST.expression) = (
  Prim AllocArray
) in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | CLOSURE ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | FIX ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | IFZ ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | LET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LOOKUP ->
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
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | CLOSURE ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | FIX ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IFZ ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LOOKUP ->
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
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState13 in
        let _v : (ClovisAST.expression list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression2__ _menhir_env _menhir_stack _menhir_s _v
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
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | REC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState16 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (ClovisAST.expression) = (
  Int x
) in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (ClovisAST.var) = (
  Identifier.mk ClovisIdentifier.value_kind x
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
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | LOOKUP ->
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
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
        let _v : (ClovisAST.expression) = (
  Lookup x
) in
        _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState135 | MenhirState5 | MenhirState124 | MenhirState108 | MenhirState12 | MenhirState102 | MenhirState13 | MenhirState96 | MenhirState94 | MenhirState89 | MenhirState19 | MenhirState85 | MenhirState83 | MenhirState21 | MenhirState25 | MenhirState29 | MenhirState32 | MenhirState73 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState53 | MenhirState47 | MenhirState44 | MenhirState42 | MenhirState38 | MenhirState26 | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (ClovisAST.expression) = (
  Var x
) in
        _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
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
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | CLOSURE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | FIX ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IFZ ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | LET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LOOKUP ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NEW ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | PROJ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | UID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 | MenhirState107 | MenhirState115 | MenhirState118 | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (ClovisAST.pattern) = (
  PVar x
) in
        _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (ClovisAST.expression) = (
  Float x
) in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | VAL ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | PIPE ->
            _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

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

and toplevel_declaration : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ClovisAST.declaration) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and toplevel_expression : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ClovisAST.expression) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APPLY ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | CLOSURE ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | FIX ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | IFZ ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | LET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | LOOKUP ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | NEW ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | PROJ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | UID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)



