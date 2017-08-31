
type vars = (string * value) list
and value =
| String of string
| Tuple of vars
| List of vars
| Values of value list
let rec show_vars v = CCFormat.(to_string (list (pair string_quoted (of_to_string show_value))) v)
and show_value = function
  | String s -> "String(" ^ CCFormat.(to_string string_quoted s) ^ ")"
  | _ -> ""

type stream_type = Console | Log | Target
let show_stream_type = function
  | Console -> "Console"
  | Log -> "Log"
  | Target -> "Target"

type async_type = Exec | Status | Notify
let show_async_type = function
  | Exec -> "Exec"
  | Status -> "Status"
  | Notify -> "Notify"

type gtoken = string option
let show_gtoken g = CCFormat.(to_string (opt string_quoted) g)

type result =
| Done of vars
| Connected
| OpError of string * string option
| Exit
let show_result = function
  | Done v -> "Done(" ^ show_vars v ^ ")"
  | Connected -> "Connected"
  | OpError (s1, s2) ->
    "OpError(" ^
    CCFormat.(to_string string_quoted s1) ^ ", " ^
    CCFormat.(to_string (opt string_quoted) s2) ^ ")"
  | Exit -> "Exit"

type output_record =
| Stream of stream_type * string
| Async of gtoken * async_type * string * vars
| Result of gtoken * result
let show_output_record = function
  | Stream (stream_type, s) ->
    "Stream(" ^
    show_stream_type stream_type ^ ", " ^
    CCFormat.(to_string string_quoted s) ^ ")"
  | Async (token, async_type, s, vars) ->
    "Async(" ^
    show_gtoken token ^ ", " ^
    show_async_type async_type ^ ", " ^
    CCFormat.(to_string string_quoted s) ^ ", " ^
    show_vars vars ^ ")"
  | Result (token, result) -> "Result(" ^ show_gtoken token ^ ", " ^ show_result result ^ ")"

type input_type = MI of gtoken | CLI

type input_output = Prompt | Input of input_type | Output of output_record
