/**
 * @file Adventlang grammar for tree-sitter
 * @author Kelley van Evert <hello@klve.nl>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  call: 15,
  member: 14,
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

  // // RUST grammar stuff
  // externals: $ => [
  //   // $.string_content, // stolen from the Rust tree sitter code
  //   $.pre_op_ws,
  // ],

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
    [$.lookup, $._expr],

    // I don't actually understand how these conflict could occur
    [$.while_expr, $.parenthesized_expression],
    [$.do_while_expr, $.parenthesized_expression],

    [$.assign_pattern, $.list_expr],

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
      optional($._stmt_separator),
      repeat(seq($._stmt_separator, $._stmt)),
      optional($._stmt_separator),
    ),

    line_comment: $ => seq(
      "//",
      token.immediate(prec(1, /.*/)),
      /\s*/, // (necessary because of the way I separate statements..)
    ),

    block_comment: $ => seq(
      "/*",
      optional($.comment_text),
      "*/",
      /\s*/, // (necessary because of the way I separate statements..)
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
      $.unary_expression,
      $.binary_expression,
      $._literal,
      $.identifier,
      $.list_expr,
      $.tuple_expr,
      $.anonymous_fn,
      $.parenthesized_expression,
      $.do_while_expr,
      $.while_expr,
      $.loop_expr,
      $.for_expr,

      $.member_expr,
      $.index_expr,

      $.postfix_index_expr,
      $.postfix_application,

      $.block_expr,

      $.application,
      // list
      //   | expr "(" list-elements(argument) ")" [ anonymous-fn ]
    ),

    member_expr: $ => prec.left(PREC.member, seq($._expr, optional("?"), ".", $._field_identifier)),

    index_expr: $ => prec.left(PREC.member, seq($._expr, optional("?"), "[", $._expr, "]")),

    postfix_index_expr: $ => prec.left(PREC.member, seq($._expr, optional("?"), ":", "[", $._expr, "]")),

    postfix_application: $ => prec.left(PREC.member, seq(
      field("left", $._expr),
      $.postfix_op,
      optional(field("right", $._expr)),
      repeat(seq("'", $.identifier, $._expr)),
    )),

    postfix_op: $ => seq(
      // optional($.pre_op_ws),//
      // optional("?"),
      // $.pre_op_ws,
      ":",
      field("fn", $.identifier),
    ),

    application: $ => seq(
      $.lookup, // instead of generalized `expr`, because I'm gonna have to statically type it anyway..
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

    anonymous_fn: $ => prec(PREC.closure, seq(
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

    regex_literal: $ => seq(
      "/",
      $.regex_pattern,
      token.immediate("/"),
      optional($.regex_flags)
    ),

    regex_pattern: $ => repeat1(choice(
      $.regex_char,
      $.regex_escape
    )),

    regex_char: $ => token.immediate(/[^/\\\n]+/),  // Any char except /, \, or newline
    regex_escape: $ => token.immediate(/\\./),  // Backslash followed by any character
    regex_flags: $ => token.immediate(/[a-zA-Z]+/),  // Common flags like 'g', 'i', 'm', etc.
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
