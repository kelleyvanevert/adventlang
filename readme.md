# Adventlang

Your friendly language that makes solving Advent of Code challenges a breeze!

_Note: This is a WORK IN PROGRESS. For the previous, working version of this language, see: https://github.com/kelleyvanevert/adventofcode2023?tab=readme-ov-file#adventlang_

![](./assets/adventlang_logo.png)

I must say, it's a bit insulting that GitHub thinks Adventlang most resembles Perl 🤔

![](./assets/github-perl-2.png)

## Grammar

It's a C-style (or more like, just basically Rust) syntax. There's parentheses, you can omit semicolons at the end of lines, most everything is an expression, etc., and then _one thing_: function application can be done infix and postfix with the ":apply syntax", which makes the language fun and easy to use for AdventOfCode problem solving. Like a swiss army knife of data transformations! For example:

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
  | "(" expr ")"

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
  "while" maybe-parenthesized([ "let" declare-pattern "=" ] expr)
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

## Issues encountered while creating the Tree-sitter parser

### `postfix_call_expr` precedence issue

When creating the `postfix_call_expr`, I had a lot of trouble getting the precedence right. I don't know exactly why or how, but the solution that I now have works, and uses a strategically placed `$._ws_preceding_arg`, to force the `optional(right_side)` to be greedy.

### `scanner.c` very confusing handling of `ERROR_SENTINEL` and `STRING_CONTENT`

I don't really know what's going on here, but, adding `STRING_CONTENT` to the external lexer, to avoid the issue of Tree-sitter skipping whitespaces as extras withing a string literal, causes problems. In the Rust tree sitter grammar, I see that they also added `ERROR_SENTINEL`, in order to specifically avoid it during recovery (of which I don't really understand how it works). Adding either of these two, or both, creates problems for the `postfix_call_expr` precedence issue. So .. I just recover skipped whitespaces in the string manually, while converting the CST to the AST in Rust.

### Newlines as stmt separators

This was a whole hassle. It now works kinda OK, but not amazing. There's some edge-cases, like parsing multiple lines of regexes, like so:

```al
/abc/
/def/
```

..because I can't differentiate it from a binary expression `a / b`.
