open Ctypes
open Foreign

open Vyos1x

module CT = Config_tree
module CD = Config_diff

let error_message = ref ""

let make_syntax_error pos err =
  match pos with
  | None -> Printf.sprintf "Syntax error: %s" err
  | Some (l, c) ->
    Printf.sprintf "Syntax error on line %d, character %d: %s" l c err

let to_json_str = fun s -> `String s

let split_on_whitespace s = Re.split (Re.Perl.compile_pat "\\s+") s

let make_config_tree name = Ctypes.Root.create (CT.make name)

let destroy c_ptr = 
    Root.release c_ptr

let from_string s = 
  try
    error_message := "";
    let config = Parser.from_string s in
    Ctypes.Root.create config
  with
    | Failure s -> error_message := s; Ctypes.null
    | Util.Syntax_error (pos, err) ->
      let msg = make_syntax_error pos err in
      error_message := msg; Ctypes.null
    | _ -> error_message := "Parse error"; Ctypes.null

let get_error () = !error_message

let render_config c_ptr =
    CT.render_config (Root.get c_ptr)

let render_json c_ptr =
    CT.render_json (Root.get c_ptr)

let render_json_ast c_ptr =
    CT.render_json_ast (Root.get c_ptr)

let render_commands c_ptr op =
    match op with
    | "delete" ->
            CT.render_commands ~op:CT.Delete (Root.get c_ptr) []
    | _ ->
            CT.render_commands ~op:CT.Set (Root.get c_ptr) []

let set_add_value c_ptr path value =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    try
        let new_ct = CT.set ct path (Some value) CT.AddValue in
        Root.set c_ptr new_ct;
        0 (* return 0 *)
    with CT.Duplicate_value -> 1

let set_replace_value c_ptr path value =
    let	ct = Root.get c_ptr in
    let	path = split_on_whitespace path in
    let new_ct = Config_tree.set ct path (Some value) Config_tree.ReplaceValue in
    Root.set c_ptr new_ct;
    0 (* return 0 *)

let set_valueless c_ptr path =
    let	ct = Root.get c_ptr in
    let	path = split_on_whitespace path in
    try
        let new_ct = Config_tree.set ct path None CT.AddValue in
        Root.set c_ptr new_ct;
        0 (* return 0 *)
    with CT.Useless_set -> 1

let delete_value c_ptr path value =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    try
        let new_ct = CT.delete ct path (Some value) in
        Root.set c_ptr new_ct;
        0 (* return 0 *)
    with CT.No_such_value -> 1

let delete_node c_ptr path =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    if not (Vytree.exists ct path) then 1 else
    let new_ct = Config_tree.delete ct path None in
    Root.set c_ptr new_ct;
    0 (* return 0 *)

let rename_node c_ptr path newname =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    if not (Vytree.exists ct path) then 1 else
    let new_ct = Vytree.rename ct path newname in
    Root.set c_ptr new_ct;
    0 (* return 0 *)

let set_tag c_ptr path =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    try
        Root.set c_ptr (CT.set_tag ct path true);
        0 (* return 0 *)
    with _ -> 1

let is_tag c_ptr path =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    if (CT.is_tag ct path) then 1 else 0

let get_subtree c_ptr path with_node =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    let subt = CT.get_subtree ~with_node:with_node ct path in
    Ctypes.Root.create subt

let exists c_ptr path =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    if (Vytree.exists ct path) then 1 else 0

let list_nodes c_ptr path =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    try
        let nodes = Vytree.children_of_path ct path in
        let nodes_json = `List (List.map to_json_str nodes) in
        Yojson.Safe.to_string nodes_json
    with _ -> Yojson.Safe.to_string `Null

let return_value c_ptr path =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    try
        Yojson.Safe.to_string (`String (CT.get_value ct path))
    with
    | CT.Node_has_no_value ->  Yojson.Safe.to_string (`String "")
    | _ -> Yojson.Safe.to_string `Null

let return_values c_ptr path =
    let ct = Root.get c_ptr in
    let path = split_on_whitespace path in
    let to_json_str = fun s -> `String s in
    try
       	let values = CT.get_values ct path in
        let values_json = `List (List.map to_json_str values) in
        Yojson.Safe.to_string values_json
    with _ -> Yojson.Safe.to_string `Null

let copy_node c_ptr old_path new_path =
    let ct = Root.get c_ptr in
    let old_path = split_on_whitespace old_path in
    let new_path = split_on_whitespace new_path in
    try
        let new_ct = Vytree.copy ct old_path new_path in
        Root.set c_ptr new_ct;
        0
    with Vytree.Nonexistent_path -> 1

let diffs path c_ptr_l c_ptr_r =
    let path = split_on_whitespace path in
    let ct_l = Root.get c_ptr_l in
    let ct_r = Root.get c_ptr_r in
    try
        let ct_add, ct_del, ct_inter = CD.diffs path ct_l ct_r in
        let ptr_add = Ctypes.Root.create ct_add in
        let ptr_del = Ctypes.Root.create ct_del in
        let ptr_inter = Ctypes.Root.create ct_inter in
        let ptr_arr = Ctypes.CArray.make (ptr void) ~initial:ptr_add 3 in Ctypes.CArray.set ptr_arr 1 ptr_del; Ctypes.CArray.set ptr_arr 2 ptr_inter; Ctypes.CArray.start ptr_arr

    with
        | CD.Incommensurable -> error_message := "Incommensurable"; Ctypes.CArray.start (Ctypes.CArray.make (ptr void) 3)
        | CD.Empty_comparison -> error_message := "Empty comparison"; Ctypes.CArray.start (Ctypes.CArray.make (ptr void) 3)

module Stubs(I : Cstubs_inverted.INTERNAL) =
struct

  let () = I.internal "make" (string @-> returning (ptr void)) make_config_tree
  let () = I.internal "destroy" ((ptr void) @-> returning void) destroy
  let () = I.internal "from_string" (string @-> returning (ptr void)) from_string
  let () = I.internal "get_error" (void @-> returning string) get_error
  let () = I.internal "to_string"  ((ptr void) @-> returning string) render_config
  let () = I.internal "to_json" ((ptr void) @-> returning string) render_json
  let () = I.internal "to_json_ast" ((ptr void) @-> returning string) render_json_ast
  let () = I.internal "to_commands" ((ptr void) @-> string @-> returning string) render_commands
  let () = I.internal "set_add_value" ((ptr void) @-> string @-> string @-> returning int) set_add_value
  let () = I.internal "set_replace_value" ((ptr void) @-> string @-> string @-> returning int) set_replace_value
  let () = I.internal "set_valueless" ((ptr void) @-> string @-> returning int) set_valueless
  let () = I.internal "delete_value" ((ptr void) @-> string @-> string @-> returning int) delete_value
  let () = I.internal "delete_node" ((ptr void) @-> string @-> returning int) delete_node
  let () = I.internal "rename_node" ((ptr void) @-> string @-> string @-> returning int) rename_node
  let () = I.internal "copy_node" ((ptr void) @-> string @-> string @-> returning int) copy_node
  let () = I.internal "set_tag" ((ptr void) @-> string @-> returning int) set_tag
  let () = I.internal "is_tag"	((ptr void) @->	string @-> returning int) is_tag
  let () = I.internal "get_subtree" ((ptr void) @-> string @-> bool @-> returning (ptr void)) get_subtree
  let () = I.internal "exists"  ((ptr void) @-> string @-> returning int) exists
  let () = I.internal "list_nodes" ((ptr void) @-> string @-> returning string) list_nodes
  let () = I.internal "return_value" ((ptr void) @-> string @-> returning string) return_value
  let () = I.internal "return_values" ((ptr void) @-> string @-> returning string) return_values
  let () = I.internal "diffs" (string @-> (ptr void) @-> (ptr void)  @-> returning (ptr (ptr void))) diffs
end
