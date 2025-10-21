#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <string.h>
#include <wctype.h>

enum TokenType {
  PRE_OP_WS,
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

static inline bool process_pre_op_ws(TSLexer *lexer) {
  lexer->mark_end(lexer);
  // bool has_content = false;

  for (;;) {
    if (lexer->lookahead == ':') {
      lexer->result_symbol = PRE_OP_WS;
      advance(lexer);
      lexer->mark_end(lexer);
      return true;
    }
    if (lexer->eof(lexer)) {
      return false;
    }
    // has_content = true;
    advance(lexer);
  }

  // return has_content;
  return false;
}

bool tree_sitter_adventlang_external_scanner_scan(void *payload, TSLexer *lexer,
                                                  const bool *valid_symbols) {
  if (valid_symbols[PRE_OP_WS]) {
    return process_pre_op_ws(lexer);
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
