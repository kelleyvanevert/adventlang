/**
 * @file Adventlang grammar for tree-sitter
 * @author Kelley van Evert <hello@klve.nl>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

// prettier-ignore
module.exports = grammar({
  name: "adventlang",

  extras: $ => [
    /\s/,
    $.line_comment,
    $.block_comment,
  ],

  word: $ => $.identifier,

  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => "hello",

    line_comment: $ => seq(
      '//',
      token.immediate(prec(1, /.*/)),
    ),

    block_comment: $ => seq(
      '/*',
      optional($.comment_text),
      '*/',
    ),

    comment_text: $ => repeat1(/.|\n|\r/),

    identifier: _ => /(r#)?[_\p{XID_Start}][_\p{XID_Continue}]*/,

    label: $ => seq('\'', $.identifier),
  },
});
