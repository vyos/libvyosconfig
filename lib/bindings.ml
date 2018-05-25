open Ctypes
open Foreign

module CT = Config_tree

let make_config_tree name = Ctypes.Root.create (Config_tree.make name)

let destroy c_ptr = 
    Root.release c_ptr

let set_add_value c_ptr path value =
    let ct = Root.get c_ptr in
    let path = Pcre.split ~rex:(Pcre.regexp "\\s+") path in
    try
        let new_ct = CT.set ct path (Some value) Config_tree.AddValue in
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
        let new_ct = Config_tree.delete ct path (Some value) in
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


module Stubs(I : Cstubs_inverted.INTERNAL) =
struct

  let () = I.internal "make" (string @-> returning (ptr void)) make_config_tree
  let () = I.internal "destroy" ((ptr void) @-> returning void) destroy
  let () = I.internal "set_add_value" ((ptr void) @-> string @-> string @-> returning int) set_add_value
  let () = I.internal "set_replace_value" ((ptr void) @-> string @-> string @-> returning int) set_replace_value
  let () = I.internal "set_valueless" ((ptr void) @-> string @-> returning int) set_valueless
  let () = I.internal "delete_value" ((ptr void) @-> string @-> string @-> returning int) delete_value
  let () = I.internal "delete_node" ((ptr void) @-> string @-> returning int) delete_node

end
