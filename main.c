#include <stdio.h>
#include <stdlib.h>

#include "parse.h"


static Slice read_file(const char *filename) {
    FILE *fp = fopen(filename, "rb");
    if (!fp) {
        return (Slice) {NULL, 0};
    }
    fseek(fp, 0, SEEK_END);
    uint32_t len = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    char *str = malloc(len + 1);
    size_t read_len = fread(str, 1, len, fp);
    fclose(fp);
    assert(len == read_len);
    str[len] = 0;
    return slice_from_str_len(str, len);
}


int main(int argc, char *argv[]) {
    init_types();
    assert(get_tuple_type(2, (Type[]) { TYPE_BOOL, TYPE_I32 }) == get_tuple_type(2, (Type[]) { TYPE_BOOL, TYPE_I32 }));
    assert(get_tuple_type(2, (Type[]) { TYPE_BOOL, TYPE_I32 }) != get_tuple_type(2, (Type[]) { TYPE_BOOL, TYPE_U32 }));
    assert(symbol_from_str("x") == symbol_from_str("x"));
    assert(symbol_from_str("x") != symbol_from_str("y"));

    Slice source = read_file("test.ml");

    ErrorCtx err_ctx;
    error_ctx_init(&err_ctx, slice_from_str("test.ml"), source);

    ParseCtx parse_ctx = {
        .err_ctx = &err_ctx,
    };

    if (parse_module(&parse_ctx, source)) {
        printf("OK!\n");
    } else {
        print_errors(&err_ctx);
    }

    /*Func *f = func_new();

    assert(get_static_i32(f, 1) == get_static_i32(f, 1));
    assert(get_static_i32(f, 1) != get_static_i32(f, 2));

    Block entry_block = create_block(f, BLOCK_NONE);
    Block then_block = create_block(f, entry_block);
    Block else_block = create_block(f, entry_block);
    Block merge_block = create_block(f, entry_block);

    emit_block(f, entry_block);
    IRRef one = get_static_i32(f, 1);
    IRRef two = get_static_i32(f, 2);
    IRRef three = get_static_i32(f, 3);
    IRRef sum = emit_binop(f, OP_ADD, one, two);
    IRRef cond = emit_binop(f, OP_EQ, sum, three);
    emit_jfalse(f, cond, else_block);
    emit_jump(f, then_block);

    emit_block(f, then_block);
    IRRef then_result = get_static_i32(f, 123);
    emit_jump(f, merge_block);
    
    emit_block(f, else_block);
    IRRef else_result = get_static_i32(f, 666);
    emit_jump(f, merge_block);

    emit_block(f, merge_block);
    emit_binop(f, OP_ADD, one, two);

    //postprocess_code(f);
    print_code(f);*/

    return 0;
}
