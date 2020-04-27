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

    return 0;
}
