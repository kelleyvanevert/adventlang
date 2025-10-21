#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <string.h>
#include <wctype.h>

enum TokenType {
  WS_PRECEDING_COLON,
  WS_PRECEDING_QUESTION_MARK,
};

static const char *OPERATORS[] = {
    ":",  "*",  "/",  "%", "+", "-", "<<", ">>", "!=",
    "==", "<=", ">=", "<", ">", "^", "&&", "||", "??",
};

static bool is_operator_start(int32_t c) {
  // For performance, prefilter valid starting characters
  switch (c) {
  case ':':
  case '*':
  case '/':
  case '%':
  case '+':
  case '-':
  case '<':
  case '>':
  case '!':
  case '=':
  case '^':
  case '&':
  case '|':
  case '?':
    return true;
  default:
    return false;
  }
}

static bool match_operator(TSLexer *lexer) {
  // Try to match any operator string at current position
  for (unsigned i = 0; i < sizeof(OPERATORS) / sizeof(OPERATORS[0]); i++) {
    const char *op = OPERATORS[i];
    unsigned j = 0;
    int32_t lookahead = lexer->lookahead;
    bool match = true;

    // Peek ahead through the operator characters
    while (op[j] != '\0') {
      if (lookahead != (int32_t)op[j]) {
        match = false;
        break;
      }
      lexer->advance(lexer, false); // peek, don’t consume
      lookahead = lexer->lookahead;
      j++;
    }

    // If we matched, return true
    if (match)
      return true;

    // Reset lexer state before trying next operator
    // (Tree-sitter doesn’t support true rewind, so we can’t consume here)
  }

  return false;
}

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

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

  return false;
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

  return false;
}

bool tree_sitter_adventlang_external_scanner_scan(void *payload, TSLexer *lexer,
                                                  const bool *valid_symbols) {

  bool done = false;

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
