#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <string.h>
#include <wctype.h>

enum TokenType {
  WS_PRECEDING_COLON,
  WS_PRECEDING_QUESTION_MARK,
  WS_PRECEDING_BINOP,
  WS_PRECEDING_ARG,
};

static const char SINGLE_CHAR_OPS[] = {'^', '/', '+', '*', '-', '>',
                                       '<', '=', '?', ':', '%'};

static const char *DOUBLE_CHAR_OPS[] = {
    // logical
    "&&", "||", "??",

    // bitwise
    ".|", ".&", ".^",

    // comparison
    "==", "!=", "<=", ">=", "<<", ">>"};

static bool is_single_char_op(int32_t c) {
  for (size_t i = 0; i < sizeof(SINGLE_CHAR_OPS); i++) {
    if (SINGLE_CHAR_OPS[i] == c) {
      return true;
    }
  }

  return false;
}

static bool is_first_char_of_double_char_op(int32_t c) {
  for (size_t i = 0; i < 11; i++) {
    if (DOUBLE_CHAR_OPS[i][0] == c) {
      return true;
    }
  }

  return false;
}

static bool is_double_char_op(int32_t a, int32_t b) {
  for (size_t i = 0; i < 11; i++) {
    if (DOUBLE_CHAR_OPS[i][0] == a && DOUBLE_CHAR_OPS[i][1] == b) {
      return true;
    }
  }

  return false;
}

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

static inline bool process_ws_preceding_arg(TSLexer *lexer) {
  lexer->mark_end(lexer);

  for (;;) {
    if (lexer->lookahead == '(' || lexer->lookahead == '[') {
      lexer->mark_end(lexer);
      lexer->result_symbol = WS_PRECEDING_ARG;
      return true;
    }

    if (lexer->eof(lexer)) {
      return false;
    }

    if (!iswspace(lexer->lookahead)) {
      return false;
    }

    advance(lexer);
  }
}

static inline bool process_ws_preceding_colon(TSLexer *lexer) {
  lexer->result_symbol = WS_PRECEDING_COLON;
  lexer->mark_end(lexer);

  for (;;) {
    if (lexer->lookahead == ':') {
      lexer->mark_end(lexer);
      return true;
    }

    if (lexer->eof(lexer)) {
      return false;
    }

    if (!iswspace(lexer->lookahead)) {
      return false;
    }

    advance(lexer);
  }
}

static inline bool process_ws_preceding_question_mark(TSLexer *lexer) {
  lexer->result_symbol = WS_PRECEDING_QUESTION_MARK;
  lexer->mark_end(lexer);

  for (;;) {
    if (lexer->lookahead == '?') {
      lexer->mark_end(lexer);
      return true;
    }

    if (lexer->eof(lexer)) {
      return false;
    }

    if (!iswspace(lexer->lookahead)) {
      return false;
    }

    advance(lexer);
  }
}

static inline bool process_ws_preceding_binop(TSLexer *lexer) {
  for (;;) {
    if (is_single_char_op(lexer->lookahead)) {
      lexer->result_symbol = WS_PRECEDING_BINOP;
      lexer->mark_end(lexer);
      return true;
    }

    if (is_first_char_of_double_char_op(lexer->lookahead)) {
      int32_t a = lexer->lookahead;
      lexer->mark_end(lexer);
      advance(lexer);
      if (lexer->eof(lexer)) {
        return false;
      }

      int32_t b = lexer->lookahead;

      if (is_double_char_op(a, b)) {
        lexer->result_symbol = WS_PRECEDING_BINOP;
        return true;
      } else {
        return false;
      }
    }

    if (lexer->eof(lexer)) {
      return false;
    }

    if (!iswspace(lexer->lookahead)) {
      return false;
    }

    advance(lexer);
  }
}

bool tree_sitter_adventlang_external_scanner_scan(void *payload, TSLexer *lexer,
                                                  const bool *valid_symbols) {

  bool done = false;

  if (valid_symbols[WS_PRECEDING_ARG]) {
    done = process_ws_preceding_arg(lexer);
    if (done) {
      return true;
    }
  }

  if (valid_symbols[WS_PRECEDING_BINOP]) {
    done = process_ws_preceding_binop(lexer);
    if (done) {
      return true;
    }
  }

  if (valid_symbols[WS_PRECEDING_QUESTION_MARK]) {
    done = process_ws_preceding_question_mark(lexer);
    if (done) {
      return true;
    }
  }

  if (valid_symbols[WS_PRECEDING_COLON]) {
    done = process_ws_preceding_colon(lexer);
    if (done) {
      return true;
    }
  }

  return false;
}

unsigned tree_sitter_adventlang_external_scanner_serialize(void *payload,
                                                           char *buffer) {
  return 0;
}

void tree_sitter_adventlang_external_scanner_deserialize(void *payload,
                                                         const char *buffer,
                                                         unsigned length) {}

void *tree_sitter_adventlang_external_scanner_create() { return NULL; }

void tree_sitter_adventlang_external_scanner_destroy(void *payload) {}
