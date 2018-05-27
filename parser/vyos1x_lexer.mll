{

open Vyos1x_parser

exception Error of string

let vy_in_string = ref false

}

rule token = parse
| [' ' '\t' '\r']
    { token lexbuf }
| '\n'
    { Lexing.new_line lexbuf; if !vy_in_string then (vy_in_string := false; NEWLINE) else token lexbuf }
| '"'
    { vy_in_string := true; read_string (Buffer.create 16) lexbuf }
| '''
    { vy_in_string := true; read_single_quoted_string (Buffer.create 16) lexbuf }
| "/*"
    { vy_in_string := false; read_comment (Buffer.create 16) lexbuf }
| '{'
    { vy_in_string := false; LEFT_BRACE }
| '}'
    { vy_in_string := false; RIGHT_BRACE }
| [^ ' ' '\t' '\n' '\r' '{' '}' '[' ']' ';' '#' '"' ''' ]+ as s
    { vy_in_string := true; IDENTIFIER s}
| eof
    { EOF }
| _
{ raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_string buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\n'      { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Error (Printf.sprintf "Illegal string character: %s" (Lexing.lexeme lexbuf))) }
  | eof { raise (Error ("String is not terminated")) }

and read_single_quoted_string buf =
  parse
  | '''       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_string buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\n'      { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | [^ ''' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_single_quoted_string buf lexbuf
    }
  | _ { raise (Error (Printf.sprintf "Illegal string character: %s" (Lexing.lexeme lexbuf))) }
  | eof { raise (Error ("String is not terminated")) }

and read_comment buf =
  parse
  | "*/"
      { COMMENT (Buffer.contents buf) }
  | _
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_comment buf lexbuf
      }
