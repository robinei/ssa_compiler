#include "parse.h"

#include <stdlib.h>
#include <string.h>

unsigned long long my_strtoull(const char *str, const char **endptr, int base);
double my_strtod(const char *string, const char **endPtr);


typedef struct {
    Type type;
    Value value;
    Slice sloc; // source location
    int lvalue_env_index;
} ParseResult;

#define PARSE_RESULT_NONE ((ParseResult) { 0, 0, SLICE_EMPTY })

#define SLICE_HERE          slice_from_str_len(ctx->ptr, 1)
#define SLICE_FROM(Start)   slice_from_str_len((Start), ctx->ptr - (Start))

#define PUSH_ERROR(Category, SourceLoc, ...) \
    do { \
        error_push(ctx->err_ctx, Category, SourceLoc, __VA_ARGS__); \
        longjmp(ctx->error_jmp_buf, 1); \
    } while(0)

#define PARSE_ERR_AT(SourceLoc, ...) PUSH_ERROR(ERROR_CATEGORY_ERROR, SourceLoc, __VA_ARGS__)
#define PARSE_ERR_HERE(...) PARSE_ERR_AT(SLICE_HERE, __VA_ARGS__)
#define PARSE_ERR_FROM(Start, ...) PARSE_ERR_AT(SLICE_FROM(Start), __VA_ARGS__)



#define CHECK_TYPE(Result, Test, ...) \
    do { \
        if (!(Test)) { \
            PARSE_ERR_AT((Result).sloc, __VA_ARGS__); \
        } \
    } while(0)


enum {
    PREC_LOWEST = 1,

    PREC_ASSIGN = PREC_LOWEST,
    PREC_LOGI_OR,
    PREC_LOGI_AND,
    PREC_BW_OR,
    PREC_BW_XOR,
    PREC_BW_AND,
    PREC_EQ,
    PREC_LTGT,
    PREC_SHIFT,
    PREC_ADDSUB,
    PREC_MULDIVMOD,

    PREC_HIGHEST
};

static ParseResult parse_infix(ParseCtx *ctx, int min_precedence);


static bool is_space_char(char ch) {
    switch (ch) {
    case '\r':
    case '\n':
    case '\t':
    case ' ':
        return true;
    default:
        return false;
    }
}
static bool is_leading_ident_char(char ch) {
    return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_';
}
static bool is_non_leading_ident_char(char ch) {
    return is_leading_ident_char(ch) || (ch >= '0' && ch <= '9');
}

static bool is_reserved_word(char *str, int len) {
    switch (*str) {
    case 'a':
        if (len == 3 && !memcmp(str, "and", len)) { return true; }
        if (len == 6 && !memcmp(str, "assert", len)) { return true; }
        break;
    case 'c':
        if (len == 5 && !memcmp(str, "const", len)) { return true; }
        break;
    case 'd':
        if (len == 2 && !memcmp(str, "do", len)) { return true; }
        break;
    case 'e':
        if (len == 4 && !memcmp(str, "else", len)) { return true; }
        if (len == 4 && !memcmp(str, "elif", len)) { return true; }
        break;
    case 'f':
        if (len == 3 && !memcmp(str, "fun", len)) { return true; }
        if (len == 5 && !memcmp(str, "false", len)) { return true; }
        break;
    case 'F':
        if (len == 3 && !memcmp(str, "Fun", len)) { return true; }
        break;
    case 'i':
        if (len == 2 && !memcmp(str, "if", len)) { return true; }
        if (len == 6 && !memcmp(str, "import", len)) { return true; }
        break;
    case 'n':
        if (len == 3 && !memcmp(str, "not", len)) { return true; }
        break;
    case 'o':
        if (len == 2 && !memcmp(str, "or", len)) { return true; }
        break;
    case 'p':
        if (len == 5 && !memcmp(str, "print", len)) { return true; }
        break;
    case 's':
        if (len == 6 && !memcmp(str, "struct", len)) { return true; }
        if (len == 6 && !memcmp(str, "static", len)) { return true; }
        break;
    case 'S':
        if (len == 4 && !memcmp(str, "Self", len)) { return true; }
        break;
    case 't':
        if (len == 4 && !memcmp(str, "then", len)) { return true; }
        if (len == 4 && !memcmp(str, "true", len)) { return true; }
        break;
    case 'v':
        if (len == 3 && !memcmp(str, "var", len)) { return true; }
        break;
    case 'w':
        if (len == 5 && !memcmp(str, "while", len)) { return true; }
        break;
    }
    return false;
}

static bool match_ident(ParseCtx *ctx) {
    if (!is_leading_ident_char(*ctx->ptr)) {
        return false;
    }
    char *start = ctx->ptr;
    do {
        ++ctx->ptr;
    } while(is_non_leading_ident_char(*ctx->ptr));
    if (is_reserved_word(start, ctx->ptr - start)) {
        PARSE_ERR_FROM(start, "unexpected keyword '%.*s'", ctx->ptr - start, start);
    }
    return true;
}
static void expect_ident(ParseCtx *ctx, const char *context_info) {
    if (!match_ident(ctx)) {
        PARSE_ERR_HERE("expected identifier %s", context_info);
    }
}

static bool match_keyword(ParseCtx *ctx, const char *str) {
    assert(is_leading_ident_char(*str));
    char *ptr = ctx->ptr;
    while (*ptr && *str) {
        if (*ptr != *str) {
            return false;
        }
        ++ptr;
        ++str;
        assert(!*str || is_non_leading_ident_char(*str));
    }
    if (*str) {
        return false;
    }
    if (is_non_leading_ident_char(*ptr)) {
        return false;
    }
    ctx->ptr = ptr;
    return true;
}
static void expect_keyword(ParseCtx *ctx, const char *str, const char *context_info) {
    if (!match_keyword(ctx, str)) {
        PARSE_ERR_HERE("expected keyword '%s' %s", str, context_info);
    }
}

static bool match_str(ParseCtx *ctx, const char *str) {
    assert(*str);
    char *ptr = ctx->ptr;
    while (*ptr && *str) {
        if (*ptr != *str) {
            return false;
        }
        ++ptr;
        ++str;
    }
    if (*str) {
        return false;
    }
    ctx->ptr = ptr;
    return true;
}
static void expect_str(ParseCtx *ctx, const char *str, const char *context_info) {
    if (!match_str(ctx, str)) {
        PARSE_ERR_HERE("expected '%s' %s", str, context_info);
    }
}

static bool match_char(ParseCtx *ctx, char ch) {
    if (*ctx->ptr != ch) {
        return false;
    }
    ++ctx->ptr;
    return true;
}
static void expect_char(ParseCtx *ctx, char ch, const char *context_info) {
    if (!match_char(ctx, ch)) {
        PARSE_ERR_HERE("expected '%c' %s", ch, context_info);
    }
}

static void skip_leading_space(ParseCtx *ctx) {
    ctx->current_indent = 0;
    for (;;) {
        switch (*ctx->ptr) {
        case '\t':
            PARSE_ERR_HERE("tabs are illegal as indentation");
        case ' ':
            ++ctx->current_indent;
            ++ctx->ptr;
            continue;
        default:
            return;
        }
    }
}
static void move_to_next_line(ParseCtx *ctx) {
    assert(*ctx->ptr == '\n');
    ++ctx->ptr;
    ++ctx->line;
    ctx->line_start = ctx->ptr;
    skip_leading_space(ctx);
}
static void skip_whitespace(ParseCtx *ctx) {
    for (;;) {
        switch (*ctx->ptr) {
        case '\n':
            if (!ctx->skip_newline) {
                return;
            }
            move_to_next_line(ctx);
            continue;
        case '\r':
        case '\t':
        case ' ':
            ++ctx->ptr;
            continue;
        }
        if (!(ctx->ptr[0] == '/' && ctx->ptr[1] == '/')) {
            break;
        }
        ctx->ptr += 2;
        for (;;) {
            char ch = *ctx->ptr;
            if (ch == '\0') {
                return;
            }
            if (ch == '\n') {
                if (!ctx->skip_newline) {
                    return;
                }
                move_to_next_line(ctx);
                break;
            }
            ++ctx->ptr;
        }
    }
}
static void skip_whitespace_including_newlines(ParseCtx *ctx) {
    bool prev_skip_newline = ctx->skip_newline;
    ctx->skip_newline = true;
    skip_whitespace(ctx);
    ctx->skip_newline = prev_skip_newline;
}
static void skip_whitespace_excluding_newlines(ParseCtx *ctx) {
    bool prev_skip_newline = ctx->skip_newline;
    ctx->skip_newline = false;
    skip_whitespace(ctx);
    ctx->skip_newline = prev_skip_newline;
}


static ParseResult parse_expr(ParseCtx *ctx) {
    return parse_infix(ctx, PREC_LOWEST + 1); // disallow '=' operator
}


static ParseResult parse_string(ParseCtx *ctx) {
    char *start = ctx->ptr++; // skip initial "
    bool has_escapes = false;
    for (;;) {
        switch (*ctx->ptr) {
        case '"': {
            if (has_escapes) {
                // TODO: copy and unescape
            }
            int length = start + 1 - ctx->ptr++;
            Type type = get_ptr_type(get_array_type(length, TYPE_U8));
            return (ParseResult) { type, 0, SLICE_FROM(start) }; // TODO get Value
        }
        case '\0':
            PARSE_ERR_HERE("unexpected end of input while parsing string literal");
        case '\\':
            ctx->ptr += 2;
            has_escapes = true;
            continue;
        default:
            ++ctx->ptr;
            continue;
        }
    }
}

static ParseResult parse_number(ParseCtx *ctx) {
    char *start = ctx->ptr;
    uint64_t value = my_strtoull(ctx->ptr, (const char **)&ctx->ptr, 0);
    if (ctx->ptr > start && *ctx->ptr != '.') {
        if (is_leading_ident_char(*ctx->ptr)) {
            PARSE_ERR_HERE("illegal number suffix");
        }
        ParseResult result;
        result.sloc = SLICE_FROM(start);
        if (value <= INT64_MAX) {
            if (value <= INT32_MAX) {
                result.type = TYPE_I32;
                result.value = get_static_i32(ctx->func, (int32_t)value);
            } else {
                // TODO: prefer u32 instead of i64, if possible? allow "u" suffix?
                result.type = TYPE_I64;
                result.value = get_static_i64(ctx->func, (int64_t)value);
            }
        } else {
            result.type = TYPE_U64;
            result.value = get_static_u64(ctx->func, value);
        }
        return result;
    } else {
        double real = my_strtod(start, (const char **)&ctx->ptr);
        if (ctx->ptr == start) {
            PARSE_ERR_HERE("error parsing number");
        }
        if (is_leading_ident_char(*ctx->ptr)) {
            PARSE_ERR_HERE("illegal number suffix");
        }
        // TODO "f" suffix for F32?
        return (ParseResult) { TYPE_F64, get_static_f64(ctx->func, real), SLICE_FROM(start) };
    }
}

static ParseResult parse_expr_seq_at_indent(ParseCtx *ctx, int indent_to_keep, char *start) {
    ParseResult result = PARSE_RESULT_NONE;
    for (;;) {
        skip_whitespace(ctx);
        switch (*ctx->ptr) {
        case '\n':
            move_to_next_line(ctx);
            continue;
        case '\0':
        case ')':
        case ',':
            goto out;
        }
        if (ctx->current_indent > indent_to_keep) {
            PARSE_ERR_HERE("unexpectedly increased indentation");
        }
        if (ctx->current_indent < indent_to_keep) {
            break;
        }
        result = parse_infix(ctx, PREC_LOWEST); // allow '='
    }
out:
    if (!result.type) {
        PARSE_ERR_HERE("expected block expression");
    }
    result.sloc = SLICE_FROM(start);
    return result;
}
static ParseResult parse_expr_seq(ParseCtx *ctx, char *start) {
    bool prev_skip_newline = ctx->skip_newline;
    ctx->skip_newline = false;
    skip_whitespace(ctx);
    if (*ctx->ptr != '\n') {
        // if an expression starts on the same line as a "block starter",
        // then expect a single-line expression for this block
        ctx->skip_newline = prev_skip_newline;
        return parse_expr(ctx);
    }
    int initial_indent = ctx->current_indent;
    move_to_next_line(ctx);
    if (ctx->current_indent <= initial_indent) {
        PARSE_ERR_HERE("expected indented expression");
    }
    ParseResult result = parse_expr_seq_at_indent(ctx, ctx->current_indent, start);
    ctx->skip_newline = prev_skip_newline;
    return result;
}
static ParseResult parse_expr_seq_in_new_env(ParseCtx *ctx, char *start) {
    int prev_env_size = ctx->env_size;
    ParseResult result = parse_expr_seq(ctx, start);
    ctx->env_size = prev_env_size;
    return result;
}

static int env_lookup(ParseCtx *ctx, Symbol sym) {
    assert(sym);
    for (int i = ctx->env_size - 1; i >= 0; --i) {
        if (ctx->env[i].sym == sym) {
            return i;
        }
    }
    return 0;
}
static void push_env(ParseCtx *ctx, Symbol sym, Type type, Value value, bool is_const) {
    assert(sym);
    assert(type);
    assert(value);
    if (ctx->env_size >= ctx->env_capacity) {
        ctx->env_capacity = ctx->env_capacity ? ctx->env_capacity * 2 : 64;
        ctx->env = malloc(sizeof(EnvEntry) * ctx->env_capacity);
    }
    ctx->env[ctx->env_size++] = (EnvEntry) {
        .sym = sym,
        .type = type,
        .is_const = is_const,
    };
    define_symbol(ctx->func, get_current_block(ctx->func), sym, value);
}

static ParseResult parse_def(ParseCtx *ctx, bool is_const, char *start) {
    skip_whitespace(ctx);
    char *sym_start = ctx->ptr;
    expect_ident(ctx, "in definition");
    Symbol name_sym = symbol_from_slice(SLICE_FROM(sym_start));
    skip_whitespace(ctx);
    
    ParseResult type_result = PARSE_RESULT_NONE;
    if (match_char(ctx, ':')) {
        skip_whitespace(ctx);
        type_result = parse_expr(ctx);
        skip_whitespace(ctx);
        CHECK_TYPE(type_result, type_result.type == TYPE_TYPE, "expected type");
    }

    expect_char(ctx, '=', "in definition");
    skip_whitespace(ctx);
    ParseResult value_result = parse_expr(ctx);
    if (type_result.type) {
        CHECK_TYPE(value_result, value_result.type == type_result.type, "does not match declared type");
    }

    push_env(ctx, name_sym, value_result.type, value_result.value, is_const);
    return (ParseResult) { TYPE_UNIT, VALUE_UNIT, SLICE_FROM(start) };
}

static ParseResult parse_if(ParseCtx *ctx, bool is_elif, char *start) {
    bool prev_skip_newline = ctx->skip_newline;
    ctx->skip_newline = true;
    skip_whitespace(ctx);
    ParseResult pred_result = parse_expr(ctx);
    skip_whitespace(ctx);
    CHECK_TYPE(pred_result, pred_result.type == TYPE_BOOL, "'if' condition must have type Bool");

    int curr_skip_emit = ctx->skip_emit;
    Block then_block = 0, else_block = 0, merge_block = 0;
    if (!curr_skip_emit) {
        if (VALUE_IS_STATIC(pred_result.value)) {
            if (pred_result.value == VALUE_FALSE) {
                ++ctx->skip_emit;
            }
        } else {
            then_block = create_block(ctx->func);
            else_block = create_block(ctx->func);
            merge_block = create_block(ctx->func);
            emit_jfalse(ctx->func, pred_result.value, else_block);
            seal_block(ctx->func, else_block);
            emit_jump(ctx->func, then_block);
            seal_block(ctx->func, then_block);
            set_current_block(ctx->func, then_block);
        }
    }
    
    char *then_start = ctx->ptr;
    expect_keyword(ctx, "then", "after 'if' condition");
    int then_indent = ctx->current_indent;
    ParseResult then_result = parse_expr_seq_in_new_env(ctx, then_start);
    char *after_then = ctx->ptr;
    skip_whitespace(ctx);
    ctx->skip_newline = prev_skip_newline;
    ctx->skip_emit = curr_skip_emit;
    if (ctx->current_indent > then_indent) {
        PARSE_ERR_HERE("unexpected indentation");
    }

    Value result_value = 0;
    if (!curr_skip_emit) {
        if (VALUE_IS_STATIC(pred_result.value)) {
            if (pred_result.value == VALUE_TRUE) {
                ++ctx->skip_emit;
            }
        } else {
            result_value = add_block_param(ctx->func, merge_block, then_result.type);
            emit_jump_with_args(ctx->func, merge_block, 1, (Value[]) { then_result.value });
            seal_block(ctx->func, merge_block);
            set_current_block(ctx->func, else_block);
        }
    }
    
    ParseResult else_result;
    if (ctx->current_indent == then_indent && match_keyword(ctx, "else")) {
        else_result = parse_expr_seq_in_new_env(ctx, after_then);
    } else if (ctx->current_indent == then_indent && match_keyword(ctx, "elif")) {
        else_result = parse_if(ctx, true, after_then);
    } else {
        Slice sloc = then_result.sloc;
        sloc.ptr += sloc.len;
        sloc.len = 0;
        else_result = (ParseResult) { TYPE_UNIT, VALUE_UNIT, sloc };
    }
    ctx->skip_emit = curr_skip_emit;
    CHECK_TYPE(else_result, else_result.type == then_result.type, "then and else branches must have equal types");
    
    ParseResult result = {
        then_result.type,
        result_value,
        SLICE_FROM(start)
    };
    if (!curr_skip_emit) {
        if (VALUE_IS_STATIC(pred_result.value)) {
            result.value = pred_result.value == VALUE_TRUE ? then_result.value : else_result.value;
        } else {
            emit_jump_with_args(ctx->func, merge_block, 1, (Value[]) { else_result.value });
            set_current_block(ctx->func, merge_block);
        }
    }
    return result;
}

static ParseResult parse_while(ParseCtx *ctx, char *start) {
    Block pred_block = 0, body_block = 0, after_block = 0;
    if (!ctx->skip_emit) {
        pred_block = create_block(ctx->func);
        body_block = create_block(ctx->func);
        after_block = create_block(ctx->func);
        emit_jump(ctx->func, pred_block);
        set_current_block(ctx->func, pred_block);
    }

    bool prev_skip_newline = ctx->skip_newline;
    ctx->skip_newline = true;
    skip_whitespace(ctx);
    ParseResult pred_result = parse_expr(ctx);
    skip_whitespace(ctx);
    CHECK_TYPE(pred_result, pred_result.type == TYPE_BOOL, "'while' condition must have type Bool");
    
    if (!ctx->skip_emit) {
        emit_jfalse(ctx->func, pred_result.value, after_block);
        seal_block(ctx->func, after_block);
        emit_jump(ctx->func, body_block);
        seal_block(ctx->func, body_block);
        set_current_block(ctx->func, body_block);
    }
    char *body_start = ctx->ptr;
    expect_keyword(ctx, "do", "after 'while' condition");
    int body_indent = ctx->current_indent;
    ParseResult body_result = parse_expr_seq_in_new_env(ctx, body_start);
    CHECK_TYPE(body_result, body_result.type == TYPE_UNIT, "expects 'while' body to have type Unit");
    ctx->skip_newline = prev_skip_newline;
    if (ctx->current_indent > body_indent) {
        PARSE_ERR_HERE("unexpected indentation");
    }
    
    if (!ctx->skip_emit) {
        emit_jump(ctx->func, pred_block);
        seal_block(ctx->func, pred_block);
        set_current_block(ctx->func, after_block);
    }
    return (ParseResult) {
        TYPE_UNIT,
        VALUE_UNIT,
        SLICE_FROM(start)
    };
}

static ParseResult parse_unary(ParseCtx *ctx, UnaryOp unop, char *start) {
    skip_whitespace(ctx);
    ParseResult arg = parse_infix(ctx, PREC_HIGHEST);
    switch (unop) {
    case UNOP_NEG: if (!TYPE_IS_NUM(arg.type)) { PARSE_ERR_AT(arg.sloc, "expected numeric type"); } break;
    case UNOP_NOT: if (arg.type == TYPE_BOOL) { PARSE_ERR_AT(arg.sloc, "expected bool type"); } break;
    case UNOP_BNOT: if (!TYPE_IS_INT(arg.type)) { PARSE_ERR_AT(arg.sloc, "expected integer type"); } break;
    }
    if (ctx->skip_emit) {
        return (ParseResult) { arg.type, 0, SLICE_FROM(start) };
    } else {
        Value value = emit_unop(ctx->func, unop, arg.value);
        return (ParseResult) { arg.type, value, SLICE_FROM(start) };
    }
}

static ParseResult parse_atom(ParseCtx *ctx) {
    char *start = ctx->ptr;
    switch (*start) {
    case '(': {
        ++ctx->ptr;
        bool prev_skip_newline = ctx->skip_newline;
        ctx->skip_newline = true;
        skip_whitespace(ctx);
        if (match_char(ctx, ')')) {
            ctx->skip_newline = prev_skip_newline;
            return (ParseResult) { TYPE_UNIT, VALUE_UNIT, SLICE_FROM(start) };
        } else {
            ParseResult result = parse_expr(ctx);
            skip_whitespace(ctx);
            expect_char(ctx, ')', "after expression following '('");
            ctx->skip_newline = prev_skip_newline;
            return result;
        }
        break;
    }
    case '+': {
        ++ctx->ptr;
        skip_whitespace(ctx);
        ParseResult result = parse_infix(ctx, PREC_HIGHEST);
        if (!TYPE_IS_NUM(result.type)) {
            PARSE_ERR_AT(result.sloc, "expected numeric type");
        }
        result.sloc = SLICE_FROM(start);
        return result;
    }
    case '-':
        ++ctx->ptr;
        return parse_unary(ctx, UNOP_NEG, start);
    case '~':
        ++ctx->ptr;
        return parse_unary(ctx, UNOP_BNOT, start);
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        return parse_number(ctx);
    case '"':
        return parse_string(ctx);
    /*case 'a':
        if (match_keyword(ctx, "assert")) {
            return parse_single_arg_prim(ctx, PRIM_ASSERT, start);
        }
        break;*/
    case 'c':
        if (match_keyword(ctx, "const")) {
            return parse_def(ctx, true, start);
        }
        break;
    case 'd':
        if (match_keyword(ctx, "do")) {
            return parse_expr_seq_in_new_env(ctx, start);
        }
        break;
    case 'f':
        if (match_keyword(ctx, "false")) {
            return (ParseResult) { TYPE_BOOL, VALUE_FALSE, SLICE_FROM(start) };
        }
        /*if (match_keyword(ctx, "fun")) {
            return parse_fun(ctx, false, start);
        }
        break;*/
    /*case 'F':
        if (match_keyword(ctx, "Fun")) {
            return parse_fun(ctx, true, start);
        }
        break;*/
    case 'i':
        if (match_keyword(ctx, "if")) {
            return parse_if(ctx, false, start);
        }
        /*if (match_keyword(ctx, "import")) {
            return parse_single_arg_prim(ctx, PRIM_IMPORT, start);
        }*/
        break;
    case 'n':
        if (match_keyword(ctx, "not")) {
            return parse_unary(ctx, UNOP_NOT, start);
        }
        break;
    /*case 'p':
        if (match_keyword(ctx, "print")) {
            return parse_single_arg_prim(ctx, PRIM_PRINT, start);
        }
        break;*/
    /*case 'q':
        if (match_keyword(ctx, "quote")) {
            return parse_single_arg_prim(ctx, PRIM_QUOTE, start);
        }
        break;*/
    /*case 's':
        if (match_keyword(ctx, "struct")) {
            return parse_struct(ctx, start);
        }
        if (match_keyword(ctx, "static")) {
            return parse_single_arg_prim(ctx, PRIM_STATIC, start);
        }
        if (match_keyword(ctx, "splice")) {
            return parse_single_arg_prim(ctx, PRIM_SPLICE, start);
        }
        break;*/
    /*case 'S':
        if (match_keyword(ctx, "Self")) {
            return expr_create(ctx, EXPR_SELF, SLICE_FROM(start));
        }*/
    case 't':
        if (match_keyword(ctx, "true")) {
            return (ParseResult) { TYPE_BOOL, VALUE_TRUE, SLICE_FROM(start) };
        }
        break;
    case 'v':
        if (match_keyword(ctx, "var")) {
            return parse_def(ctx, false, start);
        }
        break;
    case 'w':
        if (match_keyword(ctx, "while")) {
            return parse_while(ctx, start);
        }
        break;
    }
    
    if (match_ident(ctx)) {
        Slice name = SLICE_FROM(start);
        Symbol sym = symbol_from_slice(name);
        int env_index = env_lookup(ctx, sym);
        if (env_index > 0) {
            Value value = lookup_symbol(ctx->func, get_current_block(ctx->func), sym);
            assert(value);
            return (ParseResult) { ctx->env[env_index].type, value, name, .lvalue_env_index = env_index };
        } else {
            PARSE_ERR_AT(name, "undefined variable");
        }
    }
    PARSE_ERR_HERE("unexpected input while parsing atom");
}

static ParseResult parse_and_or_binop(ParseCtx *ctx, OpCode binop, ParseResult lhs, int rhs_precedence) {
    assert(binop == OP_AND || binop == OP_OR);
    CHECK_TYPE(lhs, lhs.type == TYPE_BOOL, "expected bool type");

    Block then_block = 0, merge_block = 0;
    int curr_skip_emit = ctx->skip_emit;
    if (binop == OP_AND) {
        if (lhs.value == VALUE_FALSE) {
            ++ctx->skip_emit;
        } else if (!ctx->skip_emit && lhs.value != VALUE_TRUE) {
            then_block = create_block(ctx->func);
            merge_block = create_block(ctx->func);
            emit_jfalse(ctx->func, lhs.value, merge_block);
            seal_block(ctx->func, merge_block);
            emit_jump(ctx->func, then_block);
            seal_block(ctx->func, then_block);
            set_current_block(ctx->func, then_block);
        }
    } else {
        if (lhs.value == VALUE_TRUE) {
            ++ctx->skip_emit;
        } else if (!ctx->skip_emit && lhs.value != VALUE_FALSE) {
            then_block = create_block(ctx->func);
            merge_block = create_block(ctx->func);
            emit_jfalse(ctx->func, lhs.value, then_block);
            seal_block(ctx->func, then_block);
            emit_jump(ctx->func, merge_block);
            seal_block(ctx->func, merge_block);
            set_current_block(ctx->func, then_block);
        }
    }

    ParseResult rhs = parse_infix(ctx, rhs_precedence);
    ctx->skip_emit = curr_skip_emit;
    CHECK_TYPE(rhs, rhs.type == TYPE_BOOL, "expected bool type");

    ParseResult result = { TYPE_BOOL, 0, SLICE_FROM(lhs.sloc.ptr) };
    if (binop == OP_AND) {
        if (lhs.value == VALUE_FALSE) { return lhs; } // short circuit
        if (lhs.value == VALUE_TRUE) { return rhs; } // simplify
        if (!ctx->skip_emit) {
            emit_jump(ctx->func, merge_block);
            set_current_block(ctx->func, merge_block);
            result.value = emit_binop(ctx->func, OP_AND, lhs.value, rhs.value);
        }
    } else {
        if (lhs.value == VALUE_TRUE) { return lhs; } // short circuit
        if (lhs.value == VALUE_FALSE) { return rhs; } // simplify
        if (!ctx->skip_emit) {
            emit_jump(ctx->func, merge_block);
            set_current_block(ctx->func, merge_block);
            result.value = emit_binop(ctx->func, OP_OR, lhs.value, rhs.value);
        }
    }
    return result;
}

static ParseResult parse_binop(ParseCtx *ctx, OpCode binop, ParseResult lhs, int rhs_precedence) {
    assert(OP_IS_BINOP(binop));
    ParseResult rhs = parse_infix(ctx, rhs_precedence);
    if (lhs.type != rhs.type) { CHECK_TYPE(rhs, lhs.type == rhs.type, "type mismatch"); }

    ParseResult result = { lhs.type, 0, SLICE_FROM(lhs.sloc.ptr) };

    if (OP_IS_EQCMP_BINOP(binop)) {  result.type = TYPE_BOOL; } // anything goes. TODO: smarter comparison?
    else if (OP_IS_RELCMP_BINOP(binop)) { CHECK_TYPE(rhs, TYPE_IS_NUM(rhs.type), "expected numeric type"); result.type = TYPE_BOOL; }
    else if (OP_IS_BOOL_BINOP(binop)) { CHECK_TYPE(rhs, rhs.type == TYPE_BOOL, "expected bool type"); }
    else if (OP_IS_INT_BINOP(binop)) { CHECK_TYPE(rhs, TYPE_IS_INT(rhs.type), "expected integer type"); }
    else if (OP_IS_NUM_BINOP(binop)) { CHECK_TYPE(rhs, TYPE_IS_NUM(rhs.type), "expected numeric type"); }
    else assert(0 && "unexpected binop");

    if (!ctx->skip_emit) {
        result.value = emit_binop(ctx->func, binop, lhs.value, rhs.value);
    }
    return result;
}

#define HANDLE_INFIX_BINOP(Precedence, Binop, SkipChars) \
    { \
        if (Precedence < min_precedence) { return result; } \
        ctx->ptr += SkipChars; \
        skip_whitespace(ctx); \
        result = parse_binop(ctx, Binop, result, Precedence + 1); \
        continue; \
    }

static ParseResult parse_infix(ParseCtx *ctx, int min_precedence) {
    ParseResult result = parse_atom(ctx);
    char *orig_ptr;

    while (true) {
        skip_whitespace(ctx);

        switch (*ctx->ptr) {
        case '=':
            if (ctx->ptr[1] == '=') HANDLE_INFIX_BINOP(PREC_EQ, OP_EQ, 2)
            else {
                if (PREC_LOWEST < min_precedence) { return result; }
                ++ctx->ptr;
                if (!(result.lvalue_env_index > 0)) {
                    PARSE_ERR_AT(result.sloc, "not a valid lvalue");
                }
                if (ctx->env[result.lvalue_env_index].is_const) {
                    PARSE_ERR_AT(result.sloc, "cannot overwrite constant");
                }
                skip_whitespace(ctx);
                ParseResult rhs = parse_expr(ctx);
                CHECK_TYPE(rhs, result.type == rhs.type, "mismatching types");
                define_symbol(ctx->func, get_current_block(ctx->func), ctx->env[result.lvalue_env_index].sym, rhs.value);
                return (ParseResult) { TYPE_UNIT, VALUE_UNIT, SLICE_FROM(result.sloc.ptr) };
            }

        case 'o':
            orig_ptr = ctx->ptr;
            if (match_keyword(ctx, "or")) {
                if (PREC_LOGI_OR < min_precedence) { ctx->ptr = orig_ptr; return result; }
                skip_whitespace(ctx);
                result = parse_and_or_binop(ctx, OP_OR, result, PREC_LOGI_OR + 1);
                continue;
            }
            else return result;

        case 'a':
            orig_ptr = ctx->ptr;
            if (match_keyword(ctx, "and")) {
                if (PREC_LOGI_AND < min_precedence) { ctx->ptr = orig_ptr; return result; }
                skip_whitespace(ctx);
                result = parse_and_or_binop(ctx, OP_AND, result, PREC_LOGI_AND + 1);
                continue;
            }
            else return result;

        case '|': HANDLE_INFIX_BINOP(PREC_BW_OR, OP_BOR, 1)

        case '^': HANDLE_INFIX_BINOP(PREC_BW_XOR, OP_BXOR, 1)

        case '&': HANDLE_INFIX_BINOP(PREC_BW_AND, OP_BAND, 1)

        case '!':
            if (ctx->ptr[1] == '=') HANDLE_INFIX_BINOP(PREC_EQ, OP_NEQ, 2)
            else return result;

        case '<':
            if (ctx->ptr[1] == '=') HANDLE_INFIX_BINOP(PREC_LTGT, OP_LTEQ, 2)
            else if (ctx->ptr[1] == '<') HANDLE_INFIX_BINOP(PREC_SHIFT, OP_BSHL, 2)
            else HANDLE_INFIX_BINOP(PREC_LTGT, OP_LT, 1)
        case '>':
            if (ctx->ptr[1] == '=') HANDLE_INFIX_BINOP(PREC_LTGT, OP_GTEQ, 2)
            else if (ctx->ptr[1] == '>') HANDLE_INFIX_BINOP(PREC_SHIFT, OP_BSHR, 2)
            else HANDLE_INFIX_BINOP(PREC_LTGT, OP_GT, 1)

        case '+': HANDLE_INFIX_BINOP(PREC_ADDSUB, OP_ADD, 1)
        case '-':
            if (ctx->ptr[1] != '>') HANDLE_INFIX_BINOP(PREC_ADDSUB, OP_SUB, 1)
            else return result;

        case '*': HANDLE_INFIX_BINOP(PREC_MULDIVMOD, OP_MUL, 1)
        case '/': HANDLE_INFIX_BINOP(PREC_MULDIVMOD, OP_DIV, 1)
        case '%': HANDLE_INFIX_BINOP(PREC_MULDIVMOD, OP_MOD, 1)

        //case '.': HANDLE_INFIX_BINOP(PREC_HIGHEST, PRIM_DOT, 1)
        case '(':
            if (PREC_HIGHEST < min_precedence) { return result; }
            PARSE_ERR_HERE("function calls not implemented");
            //++ctx->ptr;
            //result = parse_call(ctx, result);
            continue;

        default: return result;
        }
    }
}

bool parse_module(ParseCtx *ctx, Slice source_text) {
    Func *prev_func = ctx->func;
    ctx->func = func_new();
    
    assert(get_static_bool(ctx->func, true) == VALUE_TRUE);
    assert(get_static_bool(ctx->func, false) == VALUE_FALSE);

    if (setjmp(ctx->error_jmp_buf)) {
        ctx->func = prev_func;
        return false;
    }

    ctx->env_size = 1; // reserve 0 as illegal
    ctx->env_capacity = 128;
    ctx->env = malloc(sizeof(EnvEntry) * ctx->env_capacity);
    ctx->env[0] = (EnvEntry) { .sym = 0, .type = 0 };

    Block entry_block = create_block(ctx->func);
    assert(entry_block == 1);
    seal_block(ctx->func, entry_block);
    Value param = add_block_param(ctx->func, entry_block, TYPE_I32);
    set_current_block(ctx->func, entry_block);
    push_env(ctx, symbol_from_str("x"), TYPE_I32, param, true);

    ctx->line_start = ctx->ptr = source_text.ptr;
    char *start = ctx->ptr;
    skip_leading_space(ctx);
    ParseResult result = parse_expr_seq_at_indent(ctx, 0, start);
    skip_whitespace(ctx);
    if (*ctx->ptr) {
        PARSE_ERR_HERE("unexpected input in module");
    }
    emit_ret(ctx->func, result.value);
    
    printf("\nFinished code:\n");
    postprocess_code(ctx->func);
    print_code(ctx->func);
    ctx->func = prev_func;
    return true;
}
