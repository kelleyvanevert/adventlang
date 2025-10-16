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

function blockContents($) {
  return seq(
    optional($._stmt_separator),
    sepBy($._stmt_separator, $._stmt),
    optional($._stmt_separator),
  );
}

// prettier-ignore
module.exports = grammar({
  name: "adventlang",

  extras: $ => [
    /\s/,
    $.line_comment,
    $.block_comment,
  ],

  word: $ => $.identifier,

  externals: $ => [
    $.string_content, // stolen from the Rust tree sitter code
  ],

  supertypes: $ => [
    $._expr,
    $._type,
    $._stmt,
    $._literal,

    // RUST grammar stuff
    // $._literal_pattern,
    // $._declaration_statement,
    // $._pattern,
  ],

  // RUST grammar stuff
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

  conflicts: $ => [
    // the parser needs to disambiguate dynamically between assignment statements and expression statements
    // (as soon as it runs into an "=" it's an assignment, or else it runs into a stmt separator
    //   like a newline or something else, and in that case it's an expression statement)
    [$.assign_location, $._expr],

    // I don't actually understand how these conflict could occur
    [$.while_expr, $.parenthesized_expression],
    [$.do_while_expr, $.parenthesized_expression],

    // RUST grammar stuff
  //   // Local ambiguity due to anonymous types:
  //   // See https://internals.rust-lang.org/t/pre-rfc-deprecating-anonymous-parameters/3710
  //   [$._type, $._pattern],
  //   [$.scoped_identifier, $.scoped_type_identifier],
  //   [$.parameters, $._pattern],
  //   [$.parameters, $.tuple_struct_pattern],
  //   [$.type_parameters, $.for_lifetimes],
  //   [$.array_expression],
  //   [$.visibility_modifier],
  //   [$.visibility_modifier, $.scoped_identifier, $.scoped_type_identifier],
  ],

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

    identifier: $ => /(r#)?[_\p{XID_Start}][_\p{XID_Continue}]*/u,

    _type_identifier: $ => alias($.identifier, $.type_identifier),

    _field_identifier: $ => alias($.identifier, $.field_identifier),

    label: $ => seq("'", $.identifier),

    nil_type: $ => "nil",

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
      seq("[", $._type, "]"),
      "list",
    ),

    dict_type: $ => choice(
      seq("dict", "[", $._type, ",", $._type, "]"),
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
      optional(seq("->", $._type)),
    ),

    parenthesized_type: $ => seq("(", $._type, ")"),

    _stmt_separator: $ => /[ \t\r]*([;\n][ \t]*)+/,

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
      $.assign_stmt,
      $.expr_stmt,
    ),

    expr_stmt: $ => $._expr,

    named_fn_item: $ => seq(
      "fn",
      $.identifier,
      optional(seq("<", field("generic", listElements($._type)), ">")),
      seq("(", field("parameter", listElements($.declarable)), ")"),
      optional(seq("->", field("return", $._type))),
      "{",
      blockContents($),
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

    return_stmt: $ => choice(
      prec.left(seq("return", $._expr)),
      prec(-1, "return"),
    ),

    declare_stmt: $ => seq("let", $.declare_pattern, "=", $._expr),

    declare_guard: $ => "some",

    declare_pattern: $ => choice(
      seq(optional($.declare_guard), $.identifier, optional(seq(":", $._type))),
      seq("[", listElements($.declarable), optional(seq("..", $.identifier, optional(seq(":", $._type)))), "]"),
      seq("(", listElements($.declarable), optional(seq("..", $.identifier, optional(seq(":", $._type)))), ")"),
    ),

    declarable: $ => seq($.declare_pattern, optional(seq("=", $._expr))),

    assign_stmt: $ => seq(
      $.assign_pattern,
      alias(choice("=", "+=", "*=", "^=", "-=", "/=", "%=", "<<=", "??=", "[]="), $.assign_op),
      $._expr,
    ),

    assign_pattern: $ => choice(
      $.assign_location,
      seq("[", listElements($.assign_pattern), optional(seq("..", $.assign_pattern)), "]"),
      seq("(", listElements($.assign_pattern), optional(seq("..", $.assign_pattern)), ")"),
    ),

    assign_location: $ => choice(
      $.identifier,
      seq($._expr, "[", $._expr, "]"),
      seq($._expr, token.immediate("."), $.identifier),
    ),

    _expr: $ => choice(
      $.unary_expression,
      $.binary_expression,
      $._literal,
      prec.left($.identifier),
      $.tuple_expression,
      // do while
      // loop
      // for
      $.anonymous_fn,
      $.parenthesized_expression,
      $.do_while_expr,
      $.while_expr,
      $.loop_expr,
      $.for_expr,
      // list
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
    ),

    // do-while-expr ::=
    //   [ "'" identifier ":" ]
    //   "do"
    //   "{" block-contents "}"
    //   [ "while" expr ]

    do_while_expr: $ => seq(
      optional(seq($.label, ":")),
      "do",
      "{", blockContents($), "}",
      optional(seq("while", maybeParenthesized($._expr))),
    ),

    while_expr: $ => seq(
      optional(seq($.label, ":")),
      "while",
      maybeParenthesized(seq(optional(seq("let", $.declare_pattern, "=")), $._expr)),
      "{", blockContents($), "}",
    ),

    loop_expr: $ => seq(
      optional(seq($.label, ":")),
      "loop",
      "{", blockContents($), "}",
    ),

    for_expr: $ => seq(
      optional(seq($.label, ":")),
      "for",
      maybeParenthesized(seq("let", $.declare_pattern, "in", $._expr)),
      "{", blockContents($), "}",
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

    anonymous_fn: $ => seq(
      "|", listElements($.declarable), "|",
      "{", blockContents($), "}",
    ),

    tuple_expression: $ => seq(
      "(",
      seq($._expr, ","),
      repeat(seq($._expr, ",")),
      optional($._expr),
      ")",
    ),

    parenthesized_expression: $ => seq("(", $._expr, ")"),

    _literal: $ => choice(
      $.str_literal,
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

    nil_literal: $ => "nil",

    boolean_literal: $ => choice("true", "false"),

    integer_literal: $ => choice(
      /[0-9][0-9_]*/,
      /0x[0-9a-fA-F_]+/,
      /0b[01_]+/,
      /0o[0-7_]+/,
    ),

    str_literal: $ => seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        // $.string_content,
        $.string_character,
      )),
      token.immediate('"'),
    ),

    string_character: $ => token.immediate(/[^"\\]+/),

    escape_sequence: $ => token.immediate(
      seq("\\",
        choice(
          /[^xu]/,
          /u[0-9a-fA-F]{4}/,
          /u\{[0-9a-fA-F]+\}/,
          /x[0-9a-fA-F]{2}/,
        ),
      )
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

function maybeParenthesized(rule) {
  return choice(seq("(", rule, ")"), rule);
}

// postfix-op ::= "!"

// infix-op ::= "*" | "/" | "%" | "+" | "-" | "<<" | ">>" | "!=" | "==" | "<=" | ">=" | "<" | ">" | "^" | "&&" | "||" | "??"

// raw-str-literal ::= "r\"" text-content "\""

// str-literal ::= "\"" { text-content | str-interpolation } "\""

// str-interpolation ::= "{" expr "}"
