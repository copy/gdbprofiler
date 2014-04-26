
let make f s = Parser_utils.parse_buf_exn (f Gdbmi_lexer.ruleMain) (Lexing.from_string s)
let parse_output = make Gdbmi_parser.output
let parse_io = make Gdbmi_parser.input_output
