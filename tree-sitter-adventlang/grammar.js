/**
 * @file Adventlang grammar for tree-sitter
 * @author Kelley van Evert <hello@klve.nl>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  call: 15,
  field: 14,
  try: 13,
  unary: 12,
  cast: 11,
  multiplicative: 10,
  additive: 9,
  shift: 8,
  bitand: 7,
  bitxor: 6,
  bitor: 5,
  comparative: 4,
  and: 3,
  or: 2,
  range: 1,
  assign: 0,
  closure: -1,
};

const ws = optional(/[ \t\r\n]+/);

// prettier-ignore
module.exports = grammar({
  name: "adventlang",

  extras: $ => [
    /[ \t\r]/,
    $.line_comment,
    $.block_comment,
  ],

  word: $ => $.identifier,

  supertypes: $ => [
    $._expr,
    $._type,
    $._stmt,
    $._literal,
    // $._literal_pattern,
    // $._declaration_statement,
    // $._pattern,
  ],

  // inline: $ => [
  //   $._path,
  //   $._type_identifier,
  //   $._tokens,
  //   $._field_identifier,
  //   $._non_special_token,
  //   $._declaration_statement,
  //   $._reserved_identifier,
  //   $._expr_ending_with_block,
  // ],

  // conflicts: $ => [
  //   // Local ambiguity due to anonymous types:
  //   // See https://internals.rust-lang.org/t/pre-rfc-deprecating-anonymous-parameters/3710
  //   [$._type, $._pattern],
  //   [$.nil_type, $.tuple_pattern],
  //   [$.scoped_identifier, $.scoped_type_identifier],
  //   [$.parameters, $._pattern],
  //   [$.parameters, $.tuple_struct_pattern],
  //   [$.type_parameters, $.for_lifetimes],
  //   [$.array_expression],
  //   [$.visibility_modifier],
  //   [$.visibility_modifier, $.scoped_identifier, $.scoped_type_identifier],
  // ],

  rules: {
    source_file: ($) => seq(
      sepBy($._stmt_separator, $._stmt),
      optional($._stmt_separator)
    ),

    line_comment: $ => seq(
      "//",
      token.immediate(prec(1, /.*/)),
    ),

    block_comment: $ => seq(
      "/*",
      optional($.comment_text),
      "*/",
    ),

    comment_text: $ => repeat1(/.|\n|\r/),

    identifier: _ => /(r#)?[_\p{XID_Start}][_\p{XID_Continue}]*/u,

    _type_identifier: $ => alias($.identifier, $.type_identifier),

    _field_identifier: $ => alias($.identifier, $.field_identifier),

    label: $ => seq("'", $.identifier),

    nil_type: _ => "nil",

    tuple_type: $ => choice(
      seq(
        "(",
        seq($._type, ","),
        repeat(seq($._type, ",")),
        optional($._type),
        ")",
      ),
      "tuple",
    ),

    list_type: $ => choice(
      seq(
        "[",
        $._type,
        "]",
      ),
      "list",
    ),

    dict_type: $ => choice(
      seq(
        "dict",
        "[",
        $._type,
        ",",
        $._type,
        "]",
      ),
      "dict",
    ),

    nullable_type: $ => seq("?", $._type),

    fn_type: $ => seq(
      "fn",
      optional(
        seq(
          optional(seq("<", listElements($._type_identifier), ">")),
          "(", listElements($._type), ")"
        ),
      ),
      optional(
        seq("->", $._type),
      ),
    ),

    parenthesized_type: $ => seq("(", $._type, ")"),

    _stmt_separator: _ => /[ \t\r]*([;\n][ \t]*)+/,

    _type: $ => choice(
      $.tuple_type,
      $.list_type,
      $.dict_type,
      $.nil_type,
      alias(choice("bool", "str", "int", "float", "num", "regex"), $.primitive_type),
      $.fn_type,
      $.nullable_type,
      $._type_identifier,
      $.parenthesized_type,
    ),

    _stmt: $ => choice(
      $.named_fn_item,
      $.continue_stmt,
      $.break_stmt,
      $.return_stmt,
      $.declare_stmt,
      $._expr,
    ),

    named_fn_item: $ => seq(
      "fn", ws,
      $.identifier, ws,
      optional(seq("<", field("generic", listElements($._type)), ">")),
      seq("(", field("parameter", listElements($.declarable)), ")"),
      optional(seq("->", field("return", $._type))),
      "{",
      seq(
        sepBy($._stmt_separator, $._stmt),
        optional($._stmt_separator)
      ),
      "}",
    ),

    continue_stmt: $ => seq(
      "continue",
      optional($.label),
    ),

    break_stmt: $ => seq(
      "break",
      optional($.label),
      optional(seq("with", $._expr)),
    ),

    return_stmt: $ => seq(
      "return",
      optional($._expr),
    ),

    declare_stmt: $ => seq(
      "let",
      $.declare_pattern,
      "=",
      $._expr,
    ),

    declare_guard: $ => "some",

    declare_pattern: $ => choice(
      seq(optional($.declare_guard), $.identifier, optional(seq(":", $._type))),
      seq("[", listElements($.declarable), optional(seq("..", $.identifier, optional(seq(":", $._type)))), "]"),
      seq("(", listElements($.declarable), optional(seq("..", $.identifier, optional(seq(":", $._type)))), ")"),
    ),

    declarable: $ => seq($.declare_pattern, optional(seq("=", $._expr))),

    _expr: $ => choice(
      $.unary_expression,
      $.binary_expression,
      $._literal,
      prec.left($.identifier),
      $.tuple_expression,
      $.parenthesized_expression,
      // do while
      // while
      // loop
      // for
      // anonymous fn
      // list
    ),

    unary_expression: $ => prec(PREC.unary, seq(
      choice("-", "*", "!"),
      $._expr,
    )),

    binary_expression: $ => {
      const table = [
        [PREC.and, "&&"],
        [PREC.or, choice("||", "??")],
        [PREC.bitand, "&"],
        [PREC.bitor, "|"],
        [PREC.bitxor, "^"],
        [PREC.comparative, choice("==", "!=", "<", "<=", ">", ">=")],
        [PREC.shift, choice("<<", ">>")],
        [PREC.additive, choice("+", "-")],
        [PREC.multiplicative, choice("*", "/", "%")],
      ];

      // @ts-ignore
      return choice(...table.map(([precedence, operator]) => prec.left(precedence, seq(
        field("left", $._expr),
        // @ts-ignore
        field("operator", operator),
        field("right", $._expr),
      ))));
    },

    tuple_expression: $ => seq(
      "(",
      seq($._expr, ","),
      repeat(seq($._expr, ",")),
      optional($._expr),
      ")",
    ),

    parenthesized_expression: $ => seq("(", $._expr, ")"),

    _literal: $ => choice(
      // str
      // raw str
      // dict
      // list
      // tuple
      // float
      // regex
      $.nil_literal,
      $.boolean_literal,
      $.integer_literal,
    ),

    nil_literal: _ => "nil",

    boolean_literal: _ => choice("true", "false"),

    integer_literal: _ => choice(
      /[0-9][0-9_]*/,
      /0x[0-9a-fA-F_]+/,
      /0b[01_]+/,
      /0o[0-7_]+/,
    ),
  },
});

function sepBy1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}

function sepBy(sep, rule) {
  return optional(sepBy1(sep, rule));
}

function listElements(rule, sep = ",") {
  return optional(seq(sepBy1(sep, rule), optional(sep)));
}

// declarable ::= declare-pattern [ "=" expr ]

// expr ::=
//   | "true"
//   | "false"
//   | "nil"
//   | raw-str-literal
//   | str-literal
//   | float
//   | int
//   | regex
//   | do-while-expr
//   | while-expr
//   | loop-expr
//   | for-expr
//   | identifier
//   | anonymous-fn
//   | "(" list-elements(expr) [ ".." expr ] ")"
//   | "[" list-elements(expr) [ ".." expr ] "]"
//   | expr [ "?" ] "[" expr "]"
//   | expr [ "?" ] "." identifier
//   | expr "(" list-elements(argument) ")" [ anonymous-fn ]
//   | expr anonymous-fn
//   | expr postfix-op
//   | expr [ "?" ] ":" "[" expr "]"
//   | expr [ "?" ] ":" identifier [ expr ] { "'" identifier expr }
//   | expr infix-op expr
//   | "(" expr ")"

// argument ::= [ identifier "=" ] expr

// postfix-op ::= "!"

// infix-op ::= "*" | "/" | "%" | "+" | "-" | "<<" | ">>" | "!=" | "==" | "<=" | ">=" | "<" | ">" | "^" | "&&" | "||" | "??"

// raw-str-literal ::= "r\"" text-content "\""

// str-literal ::= "\"" { text-content | str-interpolation } "\""

// str-interpolation ::= "{" expr "}"

// text-content ::= ? textual content without " (unless escaped) ?

// do-while-expr ::=
//   [ "'" identifier ":" ]
//   "do"
//   "{" block-contents "}"
//   [ "while" expr ]

// while-expr ::=
//   [ "'" identifier ":" ]
//   "while" maybe-parenthesized([ "let" declare-pattern "=" ] expr)
//   "{" block-contents "}"

// loop-expr ::=
//   [ "'" identifier ":" ]
//   "loop"
//   "{" block-contents "}"

// for-expr ::=
//   [ "'" identifier ":" ]
//   "for" maybe-parenthesized("let" declare-pattern "in" expr)
//   "{" block-contents "}"

// maybe-parenthesized(t) ::= t | "(" t ")"

// anonymous-fn ::=
//   "|" list-elements(declarable) "|"
//   "{" block-contents "}"

// stmt ::=
//   | continue-stmt
//   | break-stmt
//   | return-stmt
//   | declare-stmt
//   | assign-stmt
//   | expr

// continue-stmt ::= "continue" [ "'" identifier ]

// break-stmt ::= "break" [ "'" identifier ] [ "with" expr ]

// return-stmt ::= "return" [ expr ]

// declare-stmt ::= "let" declare-pattern "=" expr

// assign-stmt ::= assign-pattern assign-op expr

// assign-op ::= "=" | "+=" | "*=" | "^=" | "-=" | "/=" | "%=" | "<<=" | "??=" | "[]="

// assign-pattern ::=
//   | assign-location
//   | "[" list-elements(assign-pattern) [ ".." assign-pattern ] "]"
//   | "(" list-elements(assign-pattern) [ ".." assign-pattern ] ")"

// assign-location ::=
//   | identifier
//   | expr "[" expr "]"
//   | expr "." identifier
