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
  //   [$.unit_type, $.tuple_pattern],
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
      sepBy($.stmt_separator, $._stmt),
      optional($.stmt_separator)
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

    unit_type: _ => "nil",

    tuple_type: $ => seq(
      "(",
      sepBy1(",", $._type),
      optional(","),
      ")",
    ),

    stmt_separator: _ => /[ \t\r]*([;\n][ \t]*)+/,

    _block_contents: $ => sepBy1(/[ \t\r]*([;\n][ \t]*)+/, $._stmt),

    _type: $ => choice(
      // $.abstract_type,
      // $.reference_type,
      // $.metavariable,
      // $.pointer_type,
      // $.generic_type,
      // $.scoped_type_identifier,
      $.tuple_type,
      $.unit_type,
      // $.array_type,
      // $.function_type,
      // $._type_identifier,
      // $.macro_invocation,
      // $.never_type,
      // $.dynamic_type,
      // $.bounded_type,
      // $.removed_trait_bound,
      alias(choice("bool", "str", "int", "float", "num", "regex"), $.primitive_type),
    ),

    _stmt: $ => choice(
      // $.named_fn_item,
      $.continue_stmt,
      $.break_stmt,
      $.return_stmt,
      $._expr,
      // $.declare_stmt,
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
