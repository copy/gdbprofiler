
{
  open Gdbmi_parser

  let error callerID = failwith ("Lexer error : " ^ callerID)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit | '_' | '-' )*
let wsp = [' ' '\r' '\t']

(* extract tail of the input *)
rule ruleTail acc = parse
  | eof { acc }
  | _* as str { ruleTail (acc ^ str) lexbuf }

and ruleMain = parse
  | wsp+ { ruleMain lexbuf }

  | ident as str { IDENT str }
  | digit+ as str { TOKEN str }
  | '"'   { STRING (c_string "" lexbuf) }

  | "(gdb)" { PROMPT }

  | '{'		{ LCURLY }
  | '}'		{ RCURLY }
  | "["   { LBRACKET }
  | "]"   { RBRACKET }
  | ','   { COMMA }

  | "-"   { MINUS }
  | "*"   { ASTERISK }
  | "+"   { PLUS }
  | "="   { EQUAL }
  | "~"   { TILDE }
  | "@"   { AT }
  | "&"   { AMPERSAND }
  | "^"   { CARET }

  | eof		{ EOF }
  | _	{ error "ruleMain" }

and c_unescape = parse
  | '"'         { "\"" }
  | 'n'         { "\n" }
  | 't'         { "\t" }
  | _           { error "unrecognized escape sequence" }

and c_string acc = parse
  | '"'	        { acc }
  | eof	        { error "EOI before terminating quote" }
  | "\\"        { c_string (acc ^ c_unescape lexbuf) lexbuf }
  | [^'"' '\\']+ as s { c_string (acc^s) lexbuf }
  | _		        { error "c_string" }
