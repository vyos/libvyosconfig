(* The renderer makes two assumptions about the invariants of the config files:
   that top level nodes are never tag or leaf nodes,
   and that immediate children of tag nodes are never themselves tag nodes.

   They are true in all existing VyOS configs and configs with those invariant
   broken will never load, so these assumptions are safe to make when
   processing existing configs. In configs built from scratch, the user is
   responsible for its validness.

   The original loader behaviour with tag nodes is strange: deep down, after
   config loading, they are indistinguishable from any other nodes, but at load
   time, they fail to validate unless they are formatted as tag nodes in the
   config file, that is, "ethernet eth0 { ..." as opposed to "ethernet { eth0 { ...".

   Since Vyconf makes no distinction between normal nodes and tag nodes other than
   at set command validation and formatting time, I reused the ephemeral flag
   which is never used in VyOS 1.x for marking nodes as tag nodes at parsing time.

   Note that if non-leaf nodes have values, they will be rendered in _some_ way.
   Such a config would never appear in a live VyOS and cannot load, so this case
   is considered undefined behaviour.

 *)

module CT = Config_tree
module VT = Vytree

let make_indent indent level = String.make (level * indent) ' '

let render_values indent_str name values =
    match values with
    | [] -> Printf.sprintf "%s%s { }\n" indent_str name
    | [v] -> Printf.sprintf "%s%s \"%s\"\n" indent_str name v
    | _  -> 
        let rendered = List.map (fun s -> Printf.sprintf "%s%s \"%s\"" indent_str name s) values in
        let rendered = String.concat "\n" rendered in
        Printf.sprintf "%s\n" rendered

let render_comment indent c =
    match c with
    | None -> ""
    | Some c ->  Printf.sprintf "%s/* %s */\n" indent c

let rec render_node indent level node =
    let open CT in
    let indent_str = make_indent indent level in
    let name = VT.name_of_node node in
    let data = VT.data_of_node node in
    let is_tag = data.ephemeral (* sic! look in the parser *) in 
    let comment = render_comment indent_str data.comment in
    let values = render_values indent_str name data.values in
    let children = VT.children_of_node node in
    match children with
    | [] -> Printf.sprintf "%s%s" comment values
    | _ ->
        if is_tag then 
            begin
                let inner = List.map (render_tag_node_child indent level name) children in
                String.concat "" inner
            end
        else
            begin
                let inner = List.map (render_node indent (level + 1)) children in
                let inner = String.concat "" inner in
                Printf.sprintf "%s%s%s {\n%s%s}\n" comment indent_str name inner indent_str
            end


and render_tag_node_child indent level parent node =
    let open CT in
    let indent_str = make_indent indent level in
    let name = VT.name_of_node node in
    let data = VT.data_of_node node in
    let comment = render_comment indent_str data.comment in
    let values = render_values indent_str name data.values in
    let children = VT.children_of_node node in
    match children with
    (* This produces too much whitespace due to indent_str from values,
       but the issue is cosmetic *)
    | [] -> Printf.sprintf "%s%s%s %s" comment indent_str parent values
    | _ ->
        (* Exploiting the fact that immediate children of tag nodes are
           never themselves tag nodes *)
        let inner = List.map (render_node indent (level + 1)) children in
        let inner = String.concat "" inner in
        Printf.sprintf "%s%s%s %s {\n%s%s}\n" comment indent_str parent name inner indent_str

let render node =
    let children = Vytree.children_of_node node in
    let child_configs = List.map (render_node 4 0) children in
    String.concat "" child_configs
     
    
