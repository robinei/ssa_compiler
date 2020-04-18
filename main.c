#include <stdio.h>
#include <stdlib.h>

#include "func.h"


int main(int argc, char *argv[]) {
    init_types();
    assert(get_tuple_type(2, (Type[]) { TYPE_BOOL, TYPE_I32 }) == get_tuple_type(2, (Type[]) { TYPE_BOOL, TYPE_I32 }));

    Func *f = func_new();

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
    emit_phi(f, 2, (IRRef[]) { then_result, else_result });
    emit_binop(f, OP_ADD, one, two);

    //postprocess_code(f);
    print_code(f);

    return 0;
}
