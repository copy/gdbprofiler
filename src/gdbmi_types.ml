
type vars = (string * value) list
and value =
| String of string
| Tuple of vars
| List of vars
| Values of value list
[@@deriving show]

type stream_type = Console | Log | Target
[@@deriving show]

type async_type = Exec | Status | Notify
[@@deriving show]

type gtoken = string option
[@@deriving show]

type result =
| Done of vars
| Connected
| OpError of string * string option
| Exit
[@@deriving show]

type output_record =
| Stream of stream_type * string
| Async of gtoken * async_type * string * vars
| Result of gtoken * result
[@@deriving show]

type input_type = MI of gtoken | CLI
[@@deriving show]

type input_output = Prompt | Input of input_type | Output of output_record
[@@deriving show]
