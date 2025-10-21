; Identifiers

(type_identifier) @type
(primitive_type) @type.builtin
(field_identifier) @property

; ; Identifier conventions

; Assume all-caps names are constants
((identifier) @constant
 (#match? @constant "^[A-Z][A-Z\\d_]+$'"))

; Assume uppercase names are enum constructors
((identifier) @constructor
 (#match? @constructor "^[A-Z]"))

; ; ; Function calls

(regular_call_expr
  function: (lookup (identifier) @function))

(postfix_call_expr
  function: (identifier) @function)

; ; (regular_call_expr
; ;   function: (field_expression
; ;     field: (field_identifier) @function.method))
; ; (regular_call_expr
; ;   function: (scoped_identifier
; ;     "::"
; ;     name: (identifier) @function))

; ; ; Function definitions

(named_fn_item
  name: (identifier) @function)

(line_comment) @comment
(block_comment) @comment

"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

; "::" @punctuation.delimiter
":" @punctuation.delimiter
; "." @punctuation.delimiter
; "," @punctuation.delimiter
; ";" @punctuation.delimiter

; ; (parameter (identifier) @variable.parameter)

; (label @label)

"break" @keyword
"continue" @keyword
"else" @keyword
"fn" @keyword
"for" @keyword
"if" @keyword
"in" @keyword
"let" @keyword
"return" @keyword
"while" @keyword
(declare_guard) @keyword

(str_literal) @string

(regex_literal) @string.regex

(boolean_literal) @constant.builtin
(integer_literal) @constant.builtin
(float_literal) @constant.builtin

(escape_sequence) @escape

; "*" @operator
; "&" @operator
; "'" @operator
