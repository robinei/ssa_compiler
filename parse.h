#pragma once

#include <setjmp.h>

#include "error.h"
#include "func.h"
#include "sym.h"


typedef struct {
    Symbol sym;
    Type type;
    bool is_const;
} EnvEntry;

typedef struct {
    ErrorCtx *err_ctx;
    Func *func;

    char *ptr;
    char *line_start;
    int line;
    int current_indent;
    bool skip_newline;
    int skip_emit;
    jmp_buf error_jmp_buf;

    EnvEntry *env;
    int env_size;
    int env_capacity;
} ParseCtx;

bool parse_module(ParseCtx *ctx, Slice source_text);
