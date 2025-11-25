; Identifiers

(type_identifier) @type
[
  (nil_type)
  (bool_type)
  (str_type)
  (int_type)
  (float_type)
  (num_type)
  (regex_type)
] @type.builtin
(field_identifier) @property

; ; Identifier conventions

; Assume all-caps names are constants
((identifier) @constant
 (#match? @constant "^[A-Z][A-Z\\d_]+$'"))

; Assume uppercase names are enum constructors
((identifier) @constructor
 (#match? @constructor "^[A-Z]"))

; Function calls

(regular_call_expr
  function: (identifier) @function)

(postfix_call_expr
  function: (identifier) @function)

; Function definitions

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

":" @punctuation.delimiter
; ";" @punctuation.delimiter

; (parameter (identifier) @variable.parameter)

(label) @label

"break" @keyword
"continue" @keyword
"else" @keyword
"fn" @keyword
"do" @keyword
"for" @keyword
"if" @keyword
"in" @keyword
"let" @keyword
"const" @keyword
"with" @keyword
"return" @keyword
"while" @keyword
"loop" @keyword

(str_literal) @string

(regex_literal) @string.regex

(boolean_literal) @constant.builtin
(integer_literal) @constant.builtin
(float_literal) @constant.builtin

(escape_sequence) @escape

".|" @operator
".^" @operator
".&" @operator
"=" @operator
"&&" @operator
"||" @operator
"??" @operator
"==" @operator
"!=" @operator
"<" @operator
"<=" @operator
">" @operator
">=" @operator
"<<" @operator
">>" @operator
"+" @operator
"-" @operator
"*" @operator
"/" @operator
"%" @operator
