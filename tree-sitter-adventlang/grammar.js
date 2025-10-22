/**
 * @file Adventlang grammar for tree-sitter
 * @author Kelley van Evert <hello@klve.nl>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  call: 16,
  member: 15,
  try: 14,
  unary: 13,
  cast: 12,
  exponential: 11,
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
    $._ws_preceding_binop,
    $._ws_preceding_colon,
    $._ws_preceding_question_mark,
  ],

  word: $ => $.identifier,

  externals: $ => [
    $._ws_preceding_colon,
    $._ws_preceding_question_mark,
    $._ws_preceding_binop,
  ],

  supertypes: $ => [
    $._expr,
    $._type,
    $._stmt,
    $._literal,
  ],

  conflicts: $ => [
    // the parser needs to disambiguate dynamically between assignment statements and expression statements
    // (as soon as it runs into an "=" it's an assignment, or else it runs into a stmt separator
    //   like a newline or something else, and in that case it's an expression statement)
    [$.lookup, $._expr],

    // I don't actually understand how these conflict could occur
    [$.while_expr, $.parenthesized_expression],
    [$.if_expr, $.parenthesized_expression],
    [$.do_while_expr, $.parenthesized_expression],

    [$.assign_pattern, $.list_expr],
  ],

  rules: {
    source_file: ($) => seq(
      optional($._stmt_separator),
      sepBy($._stmt_separator, $._stmt),
      optional($._stmt_separator),
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

    _stmt_separator: $ => prec(-1, /[ \t\r]*([;\n][ \t]*)+/),

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
      $.expr_stmt,
      $.assign_stmt,
    ),

    expr_stmt: $ => $._expr,

    named_fn_item: $ => seq(
      "fn",
      field("name", $.identifier),
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

    assign_pattern: $ => prec.left(PREC.assign, choice(
      $.lookup,
      seq("[", listElements($.assign_pattern), optional(seq("..", $.assign_pattern)), "]"),
      seq("(", listElements($.assign_pattern), optional(seq("..", $.assign_pattern)), ")"),
    )),

    lookup: $ => prec.dynamic(-1, choice(
      $.identifier,
      $.member_lookup,
      $.index_lookup,
    )),

    member_lookup: $ => prec.left(PREC.member, seq($.lookup, ".", $._field_identifier)),

    index_lookup: $ => prec.left(PREC.member, seq($.lookup, "[", $._expr, "]")),

    _expr: $ => choice(
      $.regular_call_expr,

      $.postfix_index_expr,
      $.postfix_call_expr,

      $._literal,
      $.list_expr,
      $.tuple_expr,
      $.anonymous_fn,
      $.unary_expression,
      $.binary_expression,
      $.parenthesized_expression,
      $.do_while_expr,
      $.while_expr,
      $.if_expr,
      $.loop_expr,
      $.for_expr,

      $.member_expr,
      $.index_expr,

      $.block_expr,

      $.identifier,
    ),

    member_expr: $ => prec.left(PREC.member, seq($._expr, optional("?"), ".", $._field_identifier)),

    index_expr: $ => prec.left(PREC.member, seq($._expr, optional("?"), "[", $._expr, "]")),

    postfix_index_expr: $ => prec.left(PREC.member, seq($._expr, optional("?"), ":", "[", $._expr, "]")),

    postfix_call_expr: $ => prec.left(PREC.member, seq(
      field("left", $._expr),

      // ugly, but it works...
      optional("?"),
      ":",

      field("function", $.identifier),
      optional(field("right", $._expr)),
      repeat(seq("'", $.identifier, $._expr)),
    )),

    regular_call_expr: $ => seq(
      field("function", $.lookup), // instead of generalized `expr`, because I'm gonna have to statically type it anyway..
      "(",
      listElements(seq(optional(seq($.identifier, "=")), $._expr)),
      ")",
    ),

    list_expr: $ => seq(
      "[",
      listElements($._expr),
      optional(seq("..", $._expr)),
      "]",
    ),

    tuple_expr: $ => seq(
      "(",
      seq($._expr, ","),
      repeat(seq($._expr, ",")),
      optional($._expr),
      ")",
    ),

    do_while_expr: $ => seq(
      optional(seq($.label, ":")),
      "do",
      $.block_expr,
      optional(seq("while", maybeParenthesized($._expr))),
    ),

    while_expr: $ => seq(
      optional(seq($.label, ":")),
      "while",
      maybeParenthesized(seq(optional(seq("let", $.declare_pattern, "=")), $._expr)),
      $.block_expr,
    ),

    if_expr: $ => seq(
      optional(seq($.label, ":")),
      "if",
      maybeParenthesized(seq(optional(seq("let", $.declare_pattern, "=")), $._expr)),
      $.block_expr,
      repeat($.else_if),
      optional($.else),
    ),

    else_if: $ => prec.left(2, seq(
      "else",
      "if",
      maybeParenthesized(seq(optional(seq("let", $.declare_pattern, "=")), $._expr)),
      $.block_expr,
    )),

    else: $ => prec.left(1, seq(
      "else",
      $.block_expr,
    )),

    loop_expr: $ => seq(
      optional(seq($.label, ":")),
      "loop",
      $.block_expr,
    ),

    for_expr: $ => seq(
      optional(seq($.label, ":")),
      "for",
      maybeParenthesized(seq("let", $.declare_pattern, "in", $._expr)),
      $.block_expr,
    ),

    block_expr: $ => seq("{", blockContents($), "}"),

    unary_expression: $ => prec(PREC.unary, seq(
      choice("-", "*", "!"),
      $._expr,
    )),

    binary_expression: $ => {
      const table = [
        [PREC.and, "&&"],
        [PREC.or, choice("||", "??")],

        // compromise syntax for now, because otherwise I can't get it to disambiguate well from anonymous functions sometimes
        [PREC.bitand, ".&"],
        [PREC.bitor, ".|"],
        [PREC.bitxor, ".^"],

        [PREC.comparative, choice("==", "!=", "<", "<=", ">", ">=")],
        [PREC.shift, choice("<<", ">>")],
        [PREC.additive, choice("+", "-")],
        [PREC.multiplicative, choice("*", "/", "%")],
        [PREC.exponential, "^"],
      ];

      // @ts-ignore
      return choice(...table.map(([precedence, operator]) => prec.left(precedence, seq(
        field("left", $._expr),
        // @ts-ignore
        field("operator", operator),
        field("right", $._expr),
      ))));
    },

    anonymous_fn: $ => prec.left(PREC.call, seq(
      choice(
        "||",
        seq("|", listElements($.declarable), "|"),
      ),
      $.block_expr,
    )),

    parenthesized_expression: $ => seq("(", $._expr, ")"),

    _literal: $ => choice(
      $.str_literal,
      // raw str
      // dict
      // float
      $.regex_literal,
      $.float_literal,
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

    float_literal: $ => token(choice(
      // Standard decimal floats
      /[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?/,     // 123.456, 123.456e10
      /[0-9]+[eE][+-]?[0-9]+/,                // 123e10, 123E-10
      /\.[0-9]+([eE][+-]?[0-9]+)?/,           // .456, .456e10

      // With underscores for readability (like in Rust/Python)
      /[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9_]+)?/,  // 1_000.456_789
      /[0-9][0-9_]*[eE][+-]?[0-9_]+/,                   // 1_000e10
      /\.[0-9][0-9_]*([eE][+-]?[0-9_]+)?/,              // .456_789
    )),

    str_literal: $ => seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        // $.string_content,
        $.string_character,
        $.string_interpolation,
      )),
      token.immediate('"'),
    ),

    string_character: $ => token.immediate(/[^"\\{]+/),

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

    string_interpolation: $ => seq("{", $._expr, "}"),

    regex_literal: $ => token(seq(
      "/",
      repeat1(choice(
        token.immediate(/[^/\\\n]+/),         // Any char except /, \, or newline
        token.immediate(/\\./),               // Backslash followed by any character
      )),
      token.immediate("/"),
      optional(token.immediate(/[a-zA-Z]+/)), // Common flags like 'g', 'i', 'm', etc.
    )),
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
