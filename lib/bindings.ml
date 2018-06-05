open Ctypes
open Foreign

module CT = Config_tree

let to_json_str = fun s -> `String s

let make_config_tree name = Ctypes.Root.create (CT.make name)

let destroy c_ptr = 
    Root.release c_ptr

let from_string s = 
    try
        let config = Vyos1x_parser.config Vyos1x_lexer.token (Lexing.from_string s) in
        Ctypes.Root.create config
    with _ -> Ctypes.null

let render c_ptr =
    Vyos1x_renderer.render (Root.get c_ptr)

let render_commands c_ptr =
    CT.render_commands ~alwayssort:true ~sortchildren:true (Root.get c_ptr) []

let set_add_value c_ptr path value =
    let ct = Root.get c_ptr in
    let path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    try
        let new_ct = CT.set ct path (Some value) CT.AddValue in
        Root.set c_ptr new_ct;
        0 (* return 0 *)
    with CT.Duplicate_value -> 1

let set_replace_value c_ptr path value =
    let	ct = Root.get c_ptr in
    let	path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    let new_ct = Config_tree.set ct path (Some value) Config_tree.ReplaceValue in
    Root.set c_ptr new_ct;
    0 (* return 0 *)

let set_valueless c_ptr path =
    let	ct = Root.get c_ptr in
    let	path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    try
        let new_ct = Config_tree.set ct path None CT.AddValue in
        Root.set c_ptr new_ct;
        0 (* return 0 *)
    with CT.Useless_set -> 1

let delete_value c_ptr path value =
    let ct = Root.get c_ptr in
    let path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    try
        let new_ct = CT.delete ct path (Some value) in
        Root.set c_ptr new_ct;
        0 (* return 0 *)
    with CT.No_such_value -> 1

let delete_node c_ptr path =
    let ct = Root.get c_ptr in
    let path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    if not (Vytree.exists ct path) then 1 else
    let new_ct = Config_tree.delete ct path None in
    Root.set c_ptr new_ct;
    0 (* return 0 *)

let set_tag c_ptr path =
    let ct = Root.get c_ptr in
    let path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    try
        Root.set c_ptr (CT.set_ephemeral ct path true);
        0 (* return 0 *)
    with _ -> 1

let is_tag c_ptr path =
    let ct = Root.get c_ptr in
    let path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    if (CT.is_ephemeral ct path) then 1 else 0

let exists c_ptr path =
    let ct = Root.get c_ptr in
    let path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    if (Vytree.exists ct path) then 1 else 0

let list_nodes c_ptr path =
    let ct = Root.get c_ptr in
    let path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    try
        let nodes = Vytree.children_of_path ct path in
        let nodes_json = `List (List.map to_json_str nodes) in
        Yojson.Safe.to_string nodes_json
    with _ -> Yojson.Safe.to_string `Null

let return_value c_ptr path =
    let ct = Root.get c_ptr in
    let path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    try
        Yojson.Safe.to_string (`String (CT.get_value ct path))
    with
    | CT.Node_has_no_value ->  Yojson.Safe.to_string (`String "")
    | _ -> Yojson.Safe.to_string `Null

let return_values c_ptr path =
    let ct = Root.get c_ptr in
    let path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    let to_json_str = fun s -> `String s in
    try
       	let values = CT.get_values ct path in
        let values_json = `List (List.map to_json_str values) in
        Yojson.Safe.to_string values_json
    with _ -> Yojson.Safe.to_string `Null


module Stubs(I : Cstubs_inverted.INTERNAL) =
struct

  let () = I.internal "make" (string @-> returning (ptr void)) make_config_tree
  let () = I.internal "destroy" ((ptr void) @-> returning void) destroy
  let () = I.internal "from_string" (string @-> returning (ptr void)) from_string 
  let () = I.internal "to_string"  ((ptr void) @-> returning string) render
  let () = I.internal "to_commands" ((ptr void) @-> returning string) render_commands
  let () = I.internal "set_add_value" ((ptr void) @-> string @-> string @-> returning int) set_add_value
  let () = I.internal "set_replace_value" ((ptr void) @-> string @-> string @-> returning int) set_replace_value
  let () = I.internal "set_valueless" ((ptr void) @-> string @-> returning int) set_valueless
  let () = I.internal "delete_value" ((ptr void) @-> string @-> string @-> returning int) delete_value
  let () = I.internal "delete_node" ((ptr void) @-> string @-> returning int) delete_node
  let () = I.internal "set_tag" ((ptr void) @-> string @-> returning int) set_tag
  let () = I.internal "is_tag"	((ptr void) @->	string @-> returning int) is_tag
  let () = I.internal "exists"  ((ptr void) @-> string @-> returning int) exists
  let () = I.internal "list_nodes" ((ptr void) @-> string @-> returning string) list_nodes
  let () = I.internal "return_value" ((ptr void) @-> string @-> returning string) return_value
  let () = I.internal "return_values" ((ptr void) @-> string @-> returning string) return_values

end
