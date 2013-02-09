exception Error

type token = 
  | THEN
  | STARDOT
  | STAR
  | SLASHDOT
  | SLASH
  | RPAREN
  | RBRACKET
  | PLUSDOT
  | PLUS
  | NEW
  | MINUSDOT
  | MINUS
  | LTDOT
  | LT
  | LPAREN
  | LET
  | LEFTARROW
  | LBRACKET
  | INT of (int)
  | IN
  | IFZ
  | ID of (string)
  | FUN
  | FLOAT of (float)
  | FID of (string)
  | EQ
  | EOF
  | ELSE
  | COMMA

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState91
  | MenhirState86
  | MenhirState83
  | MenhirState81
  | MenhirState74
  | MenhirState72
  | MenhirState66
  | MenhirState63
  | MenhirState57
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState20
  | MenhirState18
  | MenhirState17
  | MenhirState15
  | MenhirState14
  | MenhirState13
  | MenhirState11
  | MenhirState8
  | MenhirState5
  | MenhirState2
  | MenhirState0

  
  open WillowAST
  open Position

  let parse_error = Error.error "during parsing"

let _eRR =
  Error

let rec _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (WillowAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (WillowAST.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (WillowAST.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_goto_list_function_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (WillowAST.function_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | FLOAT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | IFZ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | LBRACKET ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (WillowAST.function_declaration list) =     ( x :: xs ) in
        _menhir_goto_list_function_declaration_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_compilation_unit : _menhir_env -> 'ttv_tail -> _menhir_state -> (WillowAST.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Obj.magic _1

and _menhir_goto_toplevel_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> (WillowAST.function_declaration) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Obj.magic _1

and _menhir_goto_toplevel_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Obj.magic _1

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (WillowAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, xs0) = _menhir_stack in
        let _v : (WillowAST.expression) = let es =
          let xs = xs0 in
              ( xs )
        in
        (
  BlockAlloc es
) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (WillowAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | FLOAT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | IFZ ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | LBRACKET ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | FLOAT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | IFZ ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LBRACKET ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
        let _v : (WillowAST.expression) = let b =
                     ( MulFloat )
        in
        (
  Call (Prim b, BlockAlloc [], BlockAlloc [lhs; rhs])
) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LET | MINUS | NEW | PLUS | RBRACKET | RPAREN | SLASH | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (WillowAST.expression) = let b =
                         ( Mul )
            in
            (
  Call (Prim b, BlockAlloc [], BlockAlloc [lhs; rhs])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
        let _v : (WillowAST.expression) = let b =
                     ( DivFloat )
        in
        (
  Call (Prim b, BlockAlloc [], BlockAlloc [lhs; rhs])
) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LBRACKET | LET | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PLUS | PLUSDOT | RBRACKET | RPAREN | SLASH | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (WillowAST.expression) = let b =
                         ( AddFloat )
            in
            (
  Call (Prim b, BlockAlloc [], BlockAlloc [lhs; rhs])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LBRACKET | LET | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PLUS | PLUSDOT | RBRACKET | RPAREN | SLASH | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (WillowAST.expression) = let b =
                         ( SubFloat )
            in
            (
  Call (Prim b, BlockAlloc [], BlockAlloc [lhs; rhs])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LBRACKET | LET | LPAREN | MINUS | NEW | PLUS | RBRACKET | RPAREN | SLASH | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (WillowAST.expression) = let b =
                         ( LessThanFloat )
            in
            (
  Call (Prim b, BlockAlloc [], BlockAlloc [lhs; rhs])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LBRACKET | LET | LPAREN | MINUS | NEW | PLUS | RBRACKET | RPAREN | SLASH | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (WillowAST.expression) = let b =
                         ( LessThanInt )
            in
            (
  Call (Prim b, BlockAlloc [], BlockAlloc [lhs; rhs])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | FLOAT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | IFZ ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | LBRACKET ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LET | MINUS | NEW | PLUS | RBRACKET | RPAREN | SLASH | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (WillowAST.expression) = let b =
                         ( Div )
            in
            (
  Call (Prim b, BlockAlloc [], BlockAlloc [lhs; rhs])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LEFTARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FID _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
                | FLOAT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
                | ID _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
                | IFZ ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
                | LBRACKET ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | LET ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | LPAREN ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | NEW ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
            | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LBRACKET | LET | LPAREN | LT | LTDOT | MINUS | MINUSDOT | NEW | PLUS | PLUSDOT | RBRACKET | RPAREN | SLASH | SLASHDOT | STAR | STARDOT | THEN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, e), _, i) = _menhir_stack in
                let _v : (WillowAST.expression) = (
  BlockRead (e, i)
) in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LET | NEW | RBRACKET | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e), _, i), _, v) = _menhir_stack in
            let _v : (WillowAST.expression) = (
  BlockWrite (e, i, v)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LET | MINUS | NEW | PLUS | RBRACKET | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (WillowAST.expression) = let b =
                         ( Add )
            in
            (
  Call (Prim b, BlockAlloc [], BlockAlloc [lhs; rhs])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LET | MINUS | NEW | PLUS | RBRACKET | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (WillowAST.expression) = let b =
                         ( Sub )
            in
            (
  Call (Prim b, BlockAlloc [], BlockAlloc [lhs; rhs])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e), _, env), _, x) = _menhir_stack in
            let _v : (WillowAST.expression) = (
  Call (e, env, x)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
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
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LET | NEW | RBRACKET | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, cond), _, lhs), _, rhs) = _menhir_stack in
            let _v : (WillowAST.expression) = (
  Call (Prim IfZ, BlockAlloc [], BlockAlloc [cond; lhs; rhs])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | FLOAT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | IFZ ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | LBRACKET ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (WillowAST.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | FLOAT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | IFZ ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | LBRACKET ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | IN | INT _ | LET | NEW | RBRACKET | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, x), _, lhs), _, rhs) = _menhir_stack in
            let _v : (WillowAST.expression) = (
  Let (x, lhs, rhs)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
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
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : (WillowAST.expression) = (
  e
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
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
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FID _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | FLOAT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | ID _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | IFZ ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | LBRACKET ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | LET ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | LPAREN ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | NEW ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | FLOAT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | IFZ ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | LBRACKET ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, empty), _, size), _, init) = _menhir_stack in
                let _v : (WillowAST.expression) = (
  Call (Prim AllocArray, BlockAlloc [], BlockAlloc [size; init]) 
) in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | EOF | FID _ | FLOAT _ | FUN | ID _ | IFZ | INT _ | LET | NEW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, f), _, env), _, x), _, e) = _menhir_stack in
            let _v : (WillowAST.function_declaration) = (
  (f, env, x, e)
) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState83 | MenhirState0 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FUN ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | FID _ | FLOAT _ | ID _ | IFZ | INT _ | LBRACKET | LET | LPAREN | NEW ->
                    _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
            | MenhirState86 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EOF ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, d) = _menhir_stack in
                    let _v : (WillowAST.function_declaration) = (
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
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, fdefs), _, e) = _menhir_stack in
            let _v : (WillowAST.program) = (
  (fdefs, e)
) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, p) = _menhir_stack in
                let _v : (WillowAST.program) = (
  p
) in
                _menhir_goto_compilation_unit _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _v : (WillowAST.expression) = (
  e
) in
            _menhir_goto_toplevel_expression _menhir_env _menhir_stack _menhir_s _v
        | LBRACKET ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LTDOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUSDOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUSDOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SLASHDOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | STARDOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

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

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (WillowAST.function_declaration list) =     ( [] ) in
    _menhir_goto_list_function_declaration_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState91 in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__1_ = _startpos in
        let _endpos__1_ = _endpos in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (WillowAST.expression) = (
  parse_error (Position.lex_join _startpos _endpos) "Syntax error."
) in
        _menhir_goto_toplevel_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState86 in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__1_ = _startpos in
        let _endpos__1_ = _endpos in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (WillowAST.function_declaration) = (
  parse_error (Position.lex_join _startpos _endpos) "Syntax error."
) in
        _menhir_goto_toplevel_declaration _menhir_env _menhir_stack _menhir_s _v
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let _v : (WillowAST.program) = (
  parse_error (Position.lex_join _startpos _endpos) "Syntax error."
) in
        _menhir_goto_compilation_unit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | FLOAT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | IFZ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | LBRACKET ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | LET ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | LPAREN ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState18 in
        let _v : (WillowAST.expression list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (WillowAST.expression) = (
  Int x
) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (WillowAST.var) = (
  Identifier.mk WillowIdentifier.value_kind x
) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | EQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FID _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
                | FLOAT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
                | ID _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
                | IFZ ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
                | LBRACKET ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | LET ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | LPAREN ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | NEW ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
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
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
            | FLOAT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
            | IFZ ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
            | LBRACKET ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | LPAREN ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 | MenhirState81 | MenhirState11 | MenhirState74 | MenhirState72 | MenhirState13 | MenhirState14 | MenhirState66 | MenhirState17 | MenhirState63 | MenhirState18 | MenhirState57 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState45 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState33 | MenhirState31 | MenhirState29 | MenhirState27 | MenhirState25 | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (WillowAST.expression) = (
  Var x
) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (WillowAST.expression) = (
  Float x
) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (WillowAST.fvar) = (
  Identifier.mk WillowIdentifier.value_kind x
) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
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
    | MenhirState91 | MenhirState81 | MenhirState11 | MenhirState74 | MenhirState72 | MenhirState13 | MenhirState14 | MenhirState66 | MenhirState17 | MenhirState63 | MenhirState18 | MenhirState57 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState45 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState33 | MenhirState31 | MenhirState29 | MenhirState27 | MenhirState25 | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, f) = _menhir_stack in
        let _v : (WillowAST.expression) = (
  FVar f
) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

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

and compilation_unit : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (WillowAST.program) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FID _ | FLOAT _ | ID _ | IFZ | INT _ | LBRACKET | LET | LPAREN | NEW ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and toplevel_declaration : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (WillowAST.function_declaration) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)

and toplevel_expression : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (WillowAST.expression) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | FLOAT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | IFZ ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | LBRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | LPAREN ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)



