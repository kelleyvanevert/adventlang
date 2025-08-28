# Adventlang

Your friendly language that makes solving Advent of Code challenges a breeze!

_Note: This is a WORK IN PROGRESS. For the previous, working version of this language, see: https://github.com/kelleyvanevert/adventofcode2023?tab=readme-ov-file#adventlang_

![](./assets/adventlang_logo.png)

## Grammar

It's a C-style (or more like, just basically Rust) syntax. There's parentheses, you can omit semicolons at the end of lines, most everything is an expression, etc., and then _one thing_: function application can be done infix and postfix with the ":apply syntax", which makes the whole language very easy to use for AdventOfCode problem solving. For example:

```al
let total = input :trim :lines :map |line| { line :split "-" :map int :sum } :sum
```

The formal grammar is _approximately_ like this:

```ebnf
keyword ::= "any" | "nil" | "bool" | "str" | "int" | "float" | "num" | "regex" | "tuple" | "list"

identifier ::= ? something like js identifiers ?

type-var ::= ? any identifier excl. keywords ?

list-elements(t) ::= [ t [ { "," t } [","] ] ]

type ::=
  | "nil"
  | "bool"
  | "str"
  | "int"
  | "float"
  | "num"
  | "regex"
  | "list"
  | "[" type "]"
  | "tuple"
  | "(" list-elements(type) ")"
  | "fn"
    [
      [ "<" list-elements(type-var) ">" ]
      "(" list-elements(type) ")"
    ]
    [ "->" type ]
  | "dict"
  | "?" type
  | "(" type ")"

block-contents ::= { (stmt | item) [ ";" ] }

item ::= named-fn

named-fn ::=
  "fn" identifier
  [ "<" list-elements(type-var) ">" ]
  "(" list-elements(declarable) ")"
  [ "->" type ]
  "{" block-contents "}"

declarable ::= declare-pattern [ "=" expr ]

declare-pattern ::=
  | [ "some" ] identifier [ ":" type ]
  | "[" list-elements(declarable) [ ".." identifier [ ":" type ] ] "]"
  | "(" list-elements(declarable) [ ".." identifier [ ":" type ] ] ")"

expr ::=
  | "true"
  | "false"
  | "nil"
  | raw-str-literal
  | str-literal
  | float
  | int
  | regex
  | do-while-expr
  | while-expr
  | loop-expr
  | for-expr
  | identifier
  | anonymous-fn
  | "(" list-elements(expr) [ ".." expr ] ")"
  | "[" list-elements(expr) [ ".." expr ] "]"
  | expr [ "?" ] "[" expr "]"
  | expr [ "?" ] "." identifier
  | expr "(" list-elements(argument) ")" [ anonymous-fn ]
  | expr anonymous-fn
  | expr postfix-op
  | expr [ "?" ] ":" "[" expr "]"
  | expr [ "?" ] ":" identifier [ expr ] { "'" identifier expr }
  | expr infix-op expr

argument ::= [ identifier "=" ] expr

postfix-op ::= "!"

infix-op ::= "*" | "/" | "%" | "+" | "-" | "<<" | ">>" | "!=" | "==" | "<=" | ">=" | "<" | ">" | "^" | "&&" | "||" | "??"

raw-str-literal ::= "r\"" text-content "\""

str-literal ::= "\"" { text-content | str-interpolation } "\""

str-interpolation ::= "{" expr "}"

text-content ::= ? textual content without " (unless escaped) ?

do-while-expr ::=
  [ "'" identifier ":" ]
  "do"
  "{" block-contents "}"
  [ "while" expr ]

while-expr ::= 
  [ "'" identifier ":" ]
  "while" [ "let" declare-pattern "=" ] expr
  "{" block-contents "}"

loop-expr ::=
  [ "'" identifier ":" ]
  "loop"
  "{" block-contents "}"

for-expr ::=
  [ "'" identifier ":" ]
  "for" maybe-parenthesized("let" declare-pattern "in" expr)
  "{" block-contents "}"

maybe-parenthesized(t) ::= t | "(" t ")"

anonymous-fn ::=
  "|" list-elements(declarable) "|"
  "{" block-contents "}"

stmt ::=
  | continue-stmt
  | break-stmt
  | return-stmt
  | declare-stmt
  | assign-stmt
  | expr

continue-stmt ::= "continue" [ "'" identifier ]

break-stmt ::= "break" [ "'" identifier ] [ "with" expr ]

return-stmt ::= "return" [ expr ]

declare-stmt ::= "let" declare-pattern "=" expr

assign-stmt ::= assign-pattern assign-op expr

assign-op ::= "=" | "+=" | "*=" | "^=" | "-=" | "/=" | "%=" | "<<=" | "??=" | "[]="

assign-pattern ::=
  | assign-location
  | "[" list-elements(assign-pattern) [ ".." assign-pattern ] "]"
  | "(" list-elements(assign-pattern) [ ".." assign-pattern ] ")"

assign-location ::=
  | identifier
  | expr "[" expr "]"
  | expr "." identifier
```
