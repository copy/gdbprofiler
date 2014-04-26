
type vars = (string * value) list
and value =
| String of string
| Tuple of vars
| List of vars
| Values of value list
deriving (Show)

type stream_type = Console | Log | Target
deriving (Show)

type async_type = Exec | Status | Notify
deriving (Show)

type gtoken = string option
deriving (Show)

type result =
| Done of vars
| Connected
| OpError of string * string option
| Exit
deriving (Show)

type output_record =
| Stream of stream_type * string
| Async of gtoken * async_type * string * vars
| Result of gtoken * result
deriving (Show)

type input_type = MI | CLI
deriving (Show)

type input_output = Prompt | Input of input_type | Output of output_record
deriving (Show)

let string_of_output_record = Show.show<output_record>
let string_of_value = Show.show<value>
let string_of_result = Show.show<result>
let string_of_io = Show.show<input_output>
