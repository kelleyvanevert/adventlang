/**
 * @file Adventlang grammar for tree-sitter
 * @author Kelley van Evert <hello@klve.nl>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  call: 30,
  member: 20,
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
    $._declare_pattern,
  ],

  conflicts: $ => [
    // the parser needs to disambiguate dynamically between assignment statements and expression statements
    // (as soon as it runs into an "=" it's an assignment, or else it runs into a stmt separator
    //   like a newline or something else, and in that case it's an expression statement)
    [$.lookup, $._expr],

    // I don't actually understand how these conflict could occur
    [$.while_expr, $.parenthesized_expr],
    [$.if_expr, $.parenthesized_expr],
    [$.do_while_expr, $.parenthesized_expr],

    [$.assign_list, $.list_expr],

    [$._postfix_call],
    [$._expr, $._postfix_call],
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

    tuple_type: $ => choice(
      seq(
        "(",
        seq(field("element", $._type), ","),
        repeat(seq(field("element", $._type), ",")),
        optional(field("element", $._type)),
        ")",
      ),
      "tuple",
    ),

    list_type: $ => choice(
      seq("[", field("elements", $._type), "]"),
      "list",
    ),

    dict_type: $ => choice(
      seq("dict", "[", field("key", $._type), ",", field("val", $._type), "]"),
      "dict",
    ),

    nullable_type: $ => seq("?", field("child", $._type)),

    fn_type: $ => seq(
      "fn",
      optional(
        seq(
          optional(seq("<", listElements("generic", $._type_identifier), ">")),
          "(", listElements("param", $._type), ")"
        ),
      ),
      optional(seq("->", field("return", $._type))),
    ),

    parenthesized_type: $ => seq("(", field("child", $._type), ")"),

    _stmt_separator: $ => prec(-1, /[ \t\r]*([;\n][ \t]*)+/),

    _type: $ => choice(
      $.tuple_type,
      $.list_type,
      $.dict_type,
      alias("nil", $.nil_type),
      alias("bool", $.bool_type),
      alias("str", $.str_type),
      alias("int", $.int_type),
      alias("float", $.float_type),
      alias("num", $.num_type),
      alias("regex", $.regex_type),
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
      optional(seq("<", listElements("generic", $._type_identifier), ">")),
      seq("(", listElements("param", $.declarable), ")"),
      optional(seq("->", field("return", $._type))),
      field("body", $.block_expr),
    ),

    continue_stmt: $ => seq(
      "continue",
      optional(field("label", $.label)),
    ),

    break_stmt: $ => seq(
      "break",
      optional(field("label", $.label)),
      optional(seq("with", field("expr", $._expr))),
    ),

    return_stmt: $ => choice(
      prec.left(seq("return", field("expr", $._expr))),
      prec(-1, "return"),
    ),

    declare_stmt: $ => seq("let", field("pattern", $._declare_pattern), "=", field("expr", $._expr)),

    declare_guard: $ => "some",

    _declare_pattern: $ => choice(
      $.declare_var,
      $.declare_list,
      $.declare_tuple,
    ),

    declare_var:   $ => seq(optional($.declare_guard), field("name", $.identifier), optional(seq(":", field("type", $._type)))),
    declare_list:  $ => seq("[", listElements("element", $.declarable), optional(seq("..", field("splat", $.identifier), optional(seq(":", field("splat_type", $._type))))), "]"),
    declare_tuple: $ => seq("(", listElements("element", $.declarable), optional(seq("..", field("splat", $.identifier), optional(seq(":", field("splat_type", $._type))))), ")"),

    declarable: $ => seq($._declare_pattern, optional(seq("=", field("fallback", $._expr)))),

    assign_stmt: $ => seq(
      field("pattern", $.assign_pattern),
      field("op", choice("=", "+=", "*=", "^=", "-=", "/=", "%=", "<<=", "??=", "[]=")),
      field("expr", $._expr),
    ),

    assign_pattern: $ => prec.left(PREC.assign, choice(
      $.assign_location,
      $.assign_list,
      $.assign_tuple,
    )),

    assign_location: $ => $.lookup,
    assign_list:     $ => seq("[", listElements("element", $.assign_pattern), optional(seq("..", field("splat", $.assign_pattern))), "]"),
    assign_tuple:    $ => seq("(", listElements("element", $.assign_pattern), ")"),

    lookup: $ => prec.dynamic(-1, choice(
      $.identifier,
      $.member_lookup,
      $.index_lookup,
    )),

    member_lookup: $ => prec.left(PREC.member, seq(
      field("container", $.lookup), ".", field("member", $._field_identifier)
    )),

    index_lookup: $ => prec.left(PREC.member, seq(
      field("container", $.lookup), "[", field("index", $._expr), "]"
    )),

    _expr: $ => choice(
      $.regular_call_expr,

      $.postfix_index_expr,
      $.postfix_call_expr,

      $._literal,
      $.list_expr,
      $.tuple_expr,
      $.dict_expr,
      $.anonymous_fn,
      $.unary_expression,
      $.binary_expr,
      $.parenthesized_expr,
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

    _non_binary_expr: $ => prec.right(90, choice(
      $._literal,
      $.list_expr,
      $.tuple_expr,
      $.dict_expr,
      $.anonymous_fn,
      // $.unary_expression,
      // $.binary_expr,
      $.parenthesized_expr,
      // $.do_while_expr,
      // $.while_expr,
      // $.if_expr,
      // $.loop_expr,
      // $.for_expr,

      $.member_expr,
      $.index_expr,

      // $.block_expr,

      $.identifier,
    )),

    member_expr: $ => prec.left(20, seq(
      field("container", $._expr), optional(field("coalesce", "?")), ".", field("member", $._field_identifier),
    )),

    index_expr: $ => prec.left(20, seq(
      field("container", $._expr), optional(field("coalesce", "?")), "[", field("index", $._expr), "]",
    )),

    postfix_index_expr: $ => prec.left(30, seq(
      field("container", $._expr), optional(field("coalesce", "?")), ":", "[", field("index", $._expr), "]",
    )),

    postfix_call_expr: $ => prec.left(30, seq(
      field("left", $._expr),

      optional(field("coalesce", "?")),
      ":",

      $._postfix_call,
    )),

    _postfix_call: $ => prec.dynamic(70, seq(
      field("function", $.identifier),

      optional(field("right", $._non_binary_expr)),
      repeat(field("named_arg", $.postfix_named_arg))
    )),

    postfix_named_arg: $ => seq("'", field("name", $.identifier), field("expr", $._expr)),

    regular_call_expr: $ => prec.left(25, seq(
      field("function", $._expr),
      optional(field("coalesce", "?")),
      "(",
      listElements("argument", $.call_arg),
      ")",
    )),

    call_arg: $ => seq(optional(seq(field("name", $.identifier), "=")), field("expr", $._expr)),

    list_expr: $ => seq(
      "[",
      listElements("element", $._expr),
      optional(seq("..", field("splat", $._expr))),
      "]",
    ),

    tuple_expr: $ => seq(
      "(",
      seq(field("element", $._expr), ","),
      repeat(seq(field("element", $._expr), ",")),
      optional(field("element", $._expr)),
      ")",
    ),

    dict_expr: $ => seq(
      "@{",
      listElements(
        "pair",
        $.dict_pair,
      ),
      "}",
    ),

    dict_pair: $ => choice(
      seq(".", field("id_key", $.identifier), optional(seq(":", field("val", $._expr)))),
      seq(field("expr_key", $._expr), optional(seq(":", field("val", $._expr)))),
    ),

    do_while_expr: $ => seq(
      optional(seq(field("label", $.label), ":")),
      "do",
      field("body", $.block_expr),
      optional(seq("while", maybeParenthesized(field("cond", $._expr)))),
    ),

    while_expr: $ => seq(
      optional(seq(field("label", $.label), ":")),
      "while",
      maybeParenthesized(seq(optional(seq("let", field("pattern", $._declare_pattern), "=")), field("cond", $._expr))),
      field("body", $.block_expr),
    ),

    if_expr: $ => seq(
      // optional(seq(field("label", $.label), ":")),
      "if",
      maybeParenthesized(seq(optional(seq("let", field("pattern", $._declare_pattern), "=")), field("cond", $._expr))),
      field("body", $.block_expr),
      optional(seq(
        "else",
        choice(
          field("else_if", $.if_expr),
          field("else", $.block_expr),
        ),
      )),
    ),

    loop_expr: $ => seq(
      optional(seq(field("label", $.label), ":")),
      "loop",
      field("body", $.block_expr),
    ),

    for_expr: $ => seq(
      optional(seq(field("label", $.label), ":")),
      "for",
      maybeParenthesized(seq("let", field("pattern", $._declare_pattern), "in", field("range", $._expr))),
      field("body", $.block_expr),
    ),

    block_expr: $ => seq("{", blockContents($), "}"),

    unary_expression: $ => prec(PREC.unary, seq(
      field("op", choice("-", "*", "!")),
      field("expr", $._expr),
    )),

    binary_expr: $ => {
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
        seq("|", listElements("param", $.declarable), "|"),
      ),
      field("body", $.block_expr),
    )),

    parenthesized_expr: $ => seq("(", field("child", $._expr), ")"),

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
      /-?[0-9][0-9_]*/,
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

    string_interpolation: $ => seq("{", field("interpolation", $._expr), "}"),

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

function listElements(fieldName, rule, sep = ",") {
  return optional(seq(sepBy1(sep, field(fieldName, rule)), optional(sep)));
}

function maybeParenthesized(rule) {
  return choice(seq("(", rule, ")"), rule);
}
