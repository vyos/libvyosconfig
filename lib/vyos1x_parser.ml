
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | STRING of (
# 26 "lib/vyos1x_parser.mly"
       (string)
# 11 "lib/vyos1x_parser.ml"
  )
    | RIGHT_BRACE
    | NEWLINE
    | LEFT_BRACE
    | IDENTIFIER of (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 19 "lib/vyos1x_parser.ml"
  )
    | EOF
    | COMMENT of (
# 27 "lib/vyos1x_parser.mly"
       (string)
# 25 "lib/vyos1x_parser.ml"
  )
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState21
  | MenhirState10
  | MenhirState8
  | MenhirState3
  | MenhirState1
  | MenhirState0

# 1 "lib/vyos1x_parser.mly"
  
    open Config_tree

    exception Duplicate_child of (string * string)

    (* Used for checking if after merging immediate children,
       any of them have duplicate children inside,
       e.g. "interfaces { ethernet eth0 {...} ethernet eth0 {...} }" *)
    let find_duplicate_children n =
        let rec aux xs =
            let xs = List.sort compare xs in
            match xs with
            | [] | [_] -> ()
            | x :: x' :: xs ->
                if x = x' then raise (Duplicate_child (Vytree.name_of_node n, x))
                else aux (x' :: xs)
        in
        aux @@ Vytree.list_children n

    (* When merging nodes with values, append values of subsequent nodes to the
       first one *)
    let merge_data l r = {l with values=(List.append l.values r.values)}

# 74 "lib/vyos1x_parser.ml"

let rec _menhir_goto_list_node_content_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_node_content_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * 'tv_node_content) * _menhir_state * 'tv_list_node_content_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * 'tv_node_content) * _menhir_state * 'tv_list_node_content_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_node_content)), _, (xs : 'tv_list_node_content_)) = _menhir_stack in
        let _v : 'tv_list_node_content_ = 
# 187 "/home/dmbaturin/.opam/4.05.0/lib/menhir/standard.mly"
    ( x :: xs )
# 89 "lib/vyos1x_parser.ml"
         in
        _menhir_goto_list_node_content_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv88)) : 'freshtv90)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv103 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 97 "lib/vyos1x_parser.ml"
        )) * 'tv_value)) * _menhir_state * 'tv_list_node_content_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv99 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 107 "lib/vyos1x_parser.ml"
            )) * 'tv_value)) * _menhir_state * 'tv_list_node_content_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv97 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 114 "lib/vyos1x_parser.ml"
            )) * 'tv_value)) * _menhir_state * 'tv_list_node_content_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (comment : 'tv_comments)), (name : (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 119 "lib/vyos1x_parser.ml"
            ))), (tag : 'tv_value)), _, (children : 'tv_list_node_content_)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _v : 'tv_tag_node = 
# 82 "lib/vyos1x_parser.mly"
  (
      let outer_node = Vytree.make_full {default_data with ephemeral=true} name [] in
      let inner_node =
          Vytree.make_full {default_data with comment=comment} tag [] in
      let inner_node = List.fold_left Vytree.adopt inner_node (List.rev children) |> Vytree.merge_children merge_data in
      let node = Vytree.adopt outer_node inner_node in
      try
          List.iter find_duplicate_children (Vytree.children_of_node inner_node);
          node
      with
      | Duplicate_child (child, dup) ->
          failwith (Printf.sprintf "Node \"%s %s %s\" has two children named \"%s\"" name tag child dup)
  )
# 138 "lib/vyos1x_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv95) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_tag_node) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv93) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_tag_node) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv91) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((n : 'tv_tag_node) : 'tv_tag_node) = _v in
            ((let _v : 'tv_node_content = 
# 96 "lib/vyos1x_parser.mly"
                                                                  ( n )
# 155 "lib/vyos1x_parser.ml"
             in
            _menhir_goto_node_content _menhir_env _menhir_stack _menhir_s _v) : 'freshtv92)) : 'freshtv94)) : 'freshtv96)) : 'freshtv98)) : 'freshtv100)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv101 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 165 "lib/vyos1x_parser.ml"
            )) * 'tv_value)) * _menhir_state * 'tv_list_node_content_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)) : 'freshtv104)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv119 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 174 "lib/vyos1x_parser.ml"
        ))) * _menhir_state * 'tv_list_node_content_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv115 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 184 "lib/vyos1x_parser.ml"
            ))) * _menhir_state * 'tv_list_node_content_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv113 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 191 "lib/vyos1x_parser.ml"
            ))) * _menhir_state * 'tv_list_node_content_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (comment : 'tv_comments)), (name : (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 196 "lib/vyos1x_parser.ml"
            ))), _, (children : 'tv_list_node_content_)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : 'tv_node = 
# 60 "lib/vyos1x_parser.mly"
    (
        let node =
            Vytree.make_full {default_data with comment=comment} name [] in
        let node = List.fold_left Vytree.adopt node (List.rev children) |> Vytree.merge_children merge_data in
        try
            List.iter find_duplicate_children (Vytree.children_of_node node);
            node
        with
        | Duplicate_child (child, dup) ->
            failwith (Printf.sprintf "Node \"%s %s\" has two children named \"%s\"" name child dup)
    )
# 213 "lib/vyos1x_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv111) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_node) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState3 | MenhirState0 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv105 * _menhir_state * 'tv_node) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | COMMENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
                | EOF ->
                    _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
                | IDENTIFIER _ ->
                    _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv106)
            | MenhirState8 | MenhirState21 | MenhirState10 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv109 * _menhir_state * 'tv_node) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv107 * _menhir_state * 'tv_node) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (n : 'tv_node)) = _menhir_stack in
                let _v : 'tv_node_content = 
# 96 "lib/vyos1x_parser.mly"
                       ( n )
# 246 "lib/vyos1x_parser.ml"
                 in
                _menhir_goto_node_content _menhir_env _menhir_stack _menhir_s _v) : 'freshtv108)) : 'freshtv110)
            | _ ->
                _menhir_fail ()) : 'freshtv112)) : 'freshtv114)) : 'freshtv116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv117 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 258 "lib/vyos1x_parser.ml"
            ))) * _menhir_state * 'tv_list_node_content_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)) : 'freshtv120)
    | _ ->
        _menhir_fail ()

and _menhir_goto_node_content : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_node_content -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv85 * _menhir_state * 'tv_node_content) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | RIGHT_BRACE ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | IDENTIFIER _ ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv86)

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_node_content_ = 
# 185 "/home/dmbaturin/.opam/4.05.0/lib/menhir/standard.mly"
    ( [] )
# 289 "lib/vyos1x_parser.ml"
     in
    _menhir_goto_list_node_content_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_leaf_node : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_leaf_node -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv83) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_leaf_node) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv81) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((n : 'tv_leaf_node) : 'tv_leaf_node) = _v in
    ((let _v : 'tv_node_content = 
# 96 "lib/vyos1x_parser.mly"
                                             ( n )
# 306 "lib/vyos1x_parser.ml"
     in
    _menhir_goto_node_content _menhir_env _menhir_stack _menhir_s _v) : 'freshtv82)) : 'freshtv84)

and _menhir_goto_value : _menhir_env -> 'ttv_tail -> 'tv_value -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv79 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 317 "lib/vyos1x_parser.ml"
    )) * 'tv_value) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_BRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv71 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 327 "lib/vyos1x_parser.ml"
        )) * 'tv_value) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | RIGHT_BRACE ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | IDENTIFIER _ ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv72)
    | NEWLINE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv75 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 347 "lib/vyos1x_parser.ml"
        )) * 'tv_value) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv73 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 354 "lib/vyos1x_parser.ml"
        )) * 'tv_value) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (comment : 'tv_comments)), (name : (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 359 "lib/vyos1x_parser.ml"
        ))), (value : 'tv_value)) = _menhir_stack in
        let _4 = () in
        let _v : 'tv_leaf_node = 
# 51 "lib/vyos1x_parser.mly"
    ( Vytree.make_full {default_data with values=[value]; comment=comment} name [])
# 365 "lib/vyos1x_parser.ml"
         in
        _menhir_goto_leaf_node _menhir_env _menhir_stack _menhir_s _v) : 'freshtv74)) : 'freshtv76)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv77 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 375 "lib/vyos1x_parser.ml"
        )) * 'tv_value) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)) : 'freshtv80)

and _menhir_run8 : _menhir_env -> ('ttv_tail * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 383 "lib/vyos1x_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | RIGHT_BRACE ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IDENTIFIER _ ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_list_COMMENT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_COMMENT_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state * (
# 27 "lib/vyos1x_parser.mly"
       (string)
# 413 "lib/vyos1x_parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_COMMENT_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state * (
# 27 "lib/vyos1x_parser.mly"
       (string)
# 421 "lib/vyos1x_parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_COMMENT_) : 'tv_list_COMMENT_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 27 "lib/vyos1x_parser.mly"
       (string)
# 428 "lib/vyos1x_parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_list_COMMENT_ = 
# 187 "/home/dmbaturin/.opam/4.05.0/lib/menhir/standard.mly"
    ( x :: xs )
# 433 "lib/vyos1x_parser.ml"
         in
        _menhir_goto_list_COMMENT_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv34)) : 'freshtv36)
    | MenhirState0 | MenhirState8 | MenhirState21 | MenhirState10 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_COMMENT_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((cs : 'tv_list_COMMENT_) : 'tv_list_COMMENT_) = _v in
        ((let _v : 'tv_comments = 
# 38 "lib/vyos1x_parser.mly"
                       ( match cs with [] -> None | _ -> Some (List.rev cs |> List.hd) )
# 448 "lib/vyos1x_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_comments) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState0 | MenhirState3 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv43 * _menhir_state * 'tv_comments) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENTIFIER _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_comments) = Obj.magic _menhir_stack in
                let (_v : (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 468 "lib/vyos1x_parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LEFT_BRACE ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 483 "lib/vyos1x_parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)) : 'freshtv40)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv41 * _menhir_state * 'tv_comments) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)) : 'freshtv44)
        | MenhirState8 | MenhirState21 | MenhirState10 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63 * _menhir_state * 'tv_comments) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENTIFIER _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv59 * _menhir_state * 'tv_comments) = Obj.magic _menhir_stack in
                let (_v : (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 506 "lib/vyos1x_parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENTIFIER _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
                    let (_v : (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 518 "lib/vyos1x_parser.ml"
                    )) = _v in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
                    let ((v : (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 526 "lib/vyos1x_parser.ml"
                    )) : (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 530 "lib/vyos1x_parser.ml"
                    )) = _v in
                    ((let _v : 'tv_value = 
# 44 "lib/vyos1x_parser.mly"
    ( v )
# 535 "lib/vyos1x_parser.ml"
                     in
                    _menhir_goto_value _menhir_env _menhir_stack _v) : 'freshtv46)) : 'freshtv48)
                | LEFT_BRACE ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
                | NEWLINE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 545 "lib/vyos1x_parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv49 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 552 "lib/vyos1x_parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, (comment : 'tv_comments)), (name : (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 557 "lib/vyos1x_parser.ml"
                    ))) = _menhir_stack in
                    let _3 = () in
                    let _v : 'tv_leaf_node = 
# 54 "lib/vyos1x_parser.mly"
    ( Vytree.make_full {default_data with comment=comment} name [] )
# 563 "lib/vyos1x_parser.ml"
                     in
                    _menhir_goto_leaf_node _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)) : 'freshtv52)
                | STRING _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
                    let (_v : (
# 26 "lib/vyos1x_parser.mly"
       (string)
# 572 "lib/vyos1x_parser.ml"
                    )) = _v in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv53) = Obj.magic _menhir_stack in
                    let ((v : (
# 26 "lib/vyos1x_parser.mly"
       (string)
# 580 "lib/vyos1x_parser.ml"
                    )) : (
# 26 "lib/vyos1x_parser.mly"
       (string)
# 584 "lib/vyos1x_parser.ml"
                    )) = _v in
                    ((let _v : 'tv_value = 
# 42 "lib/vyos1x_parser.mly"
    ( v )
# 589 "lib/vyos1x_parser.ml"
                     in
                    _menhir_goto_value _menhir_env _menhir_stack _v) : 'freshtv54)) : 'freshtv56)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 599 "lib/vyos1x_parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_comments) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)) : 'freshtv64)
        | _ ->
            _menhir_fail ()) : 'freshtv66)) : 'freshtv68)) : 'freshtv70)

and _menhir_goto_list_node_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_node_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * 'tv_node) * _menhir_state * 'tv_list_node_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv15 * _menhir_state * 'tv_node) * _menhir_state * 'tv_list_node_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_node)), _, (xs : 'tv_list_node_)) = _menhir_stack in
        let _v : 'tv_list_node_ = 
# 187 "/home/dmbaturin/.opam/4.05.0/lib/menhir/standard.mly"
    ( x :: xs )
# 626 "lib/vyos1x_parser.ml"
         in
        _menhir_goto_list_node_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv16)) : 'freshtv18)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_list_node_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv27 * _menhir_state * 'tv_list_node_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_list_node_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (ns : 'tv_list_node_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 33 "lib/vyos1x_parser.mly"
       (Config_tree.t)
# 645 "lib/vyos1x_parser.ml"
            ) = 
# 100 "lib/vyos1x_parser.mly"
 (
    let root = make "root" in
    let root = List.fold_left Vytree.adopt root (List.rev ns) |> Vytree.merge_children merge_data in
        try
            List.iter find_duplicate_children (Vytree.children_of_node root);
            root
        with
        | Duplicate_child (child, dup) ->
            failwith (Printf.sprintf "Node \"%s\" has two children named \"%s\"" child dup)
  )
# 658 "lib/vyos1x_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv23) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 33 "lib/vyos1x_parser.mly"
       (Config_tree.t)
# 666 "lib/vyos1x_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 33 "lib/vyos1x_parser.mly"
       (Config_tree.t)
# 674 "lib/vyos1x_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 33 "lib/vyos1x_parser.mly"
       (Config_tree.t)
# 682 "lib/vyos1x_parser.ml"
            )) : (
# 33 "lib/vyos1x_parser.mly"
       (Config_tree.t)
# 686 "lib/vyos1x_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv20)) : 'freshtv22)) : 'freshtv24)) : 'freshtv26)) : 'freshtv28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_list_node_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)) : 'freshtv32)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv3 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 707 "lib/vyos1x_parser.ml"
        )) * 'tv_value)) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv4)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state * 'tv_node_content) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv7 * _menhir_state * 'tv_comments) * (
# 25 "lib/vyos1x_parser.mly"
       (string)
# 721 "lib/vyos1x_parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state * 'tv_node) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * (
# 27 "lib/vyos1x_parser.mly"
       (string)
# 735 "lib/vyos1x_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv14)

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_COMMENT_ = 
# 185 "/home/dmbaturin/.opam/4.05.0/lib/menhir/standard.mly"
    ( [] )
# 749 "lib/vyos1x_parser.ml"
     in
    _menhir_goto_list_COMMENT_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_node_ = 
# 185 "/home/dmbaturin/.opam/4.05.0/lib/menhir/standard.mly"
    ( [] )
# 758 "lib/vyos1x_parser.ml"
     in
    _menhir_goto_list_node_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 27 "lib/vyos1x_parser.mly"
       (string)
# 765 "lib/vyos1x_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IDENTIFIER _ ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and config : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 33 "lib/vyos1x_parser.mly"
       (Config_tree.t)
# 796 "lib/vyos1x_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EOF ->
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENTIFIER _ ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 219 "/home/dmbaturin/.opam/4.05.0/lib/menhir/standard.mly"
  


# 830 "lib/vyos1x_parser.ml"
