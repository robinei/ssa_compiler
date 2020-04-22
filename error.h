#pragma once

#include <stdio.h>
#include "slice.h"

#define ERROR_FILENAME_BUF_SIZE 256
#define ERROR_MSG_BUF_SIZE 1024

typedef enum {
    ERROR_CATEGORY_MESSAGE,
    ERROR_CATEGORY_WARNING,
    ERROR_CATEGORY_ERROR,
} ErrorCategory;

typedef struct ErrorEntry {
    struct ErrorEntry *next;
    ErrorCategory category;
    Slice location;
    char message[0];
} ErrorEntry;

typedef struct {
    ErrorEntry *first_error;
    ErrorEntry *last_error;
    Slice source_buf;
    Slice filename;
    char msg_buf[ERROR_MSG_BUF_SIZE];
} ErrorCtx;

void error_ctx_init(ErrorCtx *ctx, Slice filename, Slice source_text);

void error_push(ErrorCtx *ctx, ErrorCategory category, Slice location, const char *format, ...);

Slice error_line_text(ErrorCtx *ctx, ErrorEntry *entry);
int error_line_num(ErrorCtx *ctx, ErrorEntry *entry);
int error_col_num(ErrorCtx *ctx, ErrorEntry *entry);

void error_fprint(ErrorCtx *ctx, ErrorEntry *entry, FILE *fp);

void print_errors(ErrorCtx *err_ctx);
