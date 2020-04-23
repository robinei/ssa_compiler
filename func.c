#include "func.h"
#include "fnv.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

static void print_instr(Func *f, Block block, IRRef i);

typedef union {
    struct {
        Block block;
        Symbol sym;
    };
    uint64_t u64_repr;
} EnvKey;

typedef union {
    IRRef ref;
    uint64_t u64_repr;
} EnvValue;

Func *func_new() {
    Func *f = calloc(1, sizeof(Func));
    f->blocks_size = 1; // reserve slot for BLOCK_NONE
    f->blocks_capacity = 128;
    f->blocks = calloc(1, sizeof(BlockInfo) * f->blocks_capacity);
    
    f->static_value_capacity = 128;
    f->instrs_capacity = 128;
    int buffer_capacity = f->static_value_capacity + f->instrs_capacity;
    f->static_values = (StaticValue *)calloc(1, sizeof(StaticValue) * buffer_capacity) + f->static_value_capacity;
    f->instrs = (Instr *)calloc(1, sizeof(Instr) * buffer_capacity) + f->static_value_capacity;
    f->types = (Type *)calloc(1, sizeof(Type) * buffer_capacity) + f->static_value_capacity;

    f->types[IRREF_NONE] = TYPE_NONE;
    f->types[IRREF_UNIT] = TYPE_UNIT;
    f->types[IRREF_FALSE] = TYPE_BOOL;
    f->types[IRREF_TRUE] = TYPE_BOOL;
    f->static_values[IRREF_TRUE].b = true;
    f->static_value_size = 4; // NONE/UNIT/FALSE/TRUE
    return f;
}

static void grow_buffers(Func *f, int static_value_capacity, int instrs_capacity) {
    int buffer_capacity = static_value_capacity + instrs_capacity;
    int static_size = f->static_value_size;
    int total_size = static_size + f->instrs_size;

    StaticValue *static_values = (StaticValue *)calloc(1, sizeof(StaticValue) * buffer_capacity) + static_value_capacity;
    Instr *instrs = (Instr *)calloc(1, sizeof(Instr) * buffer_capacity) + static_value_capacity;
    Type *types = (Type *)calloc(1, sizeof(Type) * buffer_capacity) + static_value_capacity;

    memcpy(static_values - static_size, f->static_values - static_size, total_size);
    memcpy(instrs - static_size, f->instrs - static_size, total_size);
    memcpy(types - static_size, f->types - static_size, total_size);

    free(f->static_values - f->static_value_capacity);
    free(f->instrs - f->static_value_capacity);
    free(f->types - f->static_value_capacity);

    f->static_values = static_values;
    f->instrs = instrs;
    f->types = types;
    f->static_value_capacity = static_value_capacity;
    f->instrs_capacity = instrs_capacity;
}

Block create_block(Func *f, Block idom_block) {
    assert(idom_block == BLOCK_NONE || idom_block < f->blocks_size);
    assert(idom_block != BLOCK_NONE || f->blocks_size == 1);

    if (f->blocks_size >= f->blocks_capacity) {
        f->blocks_capacity = f->blocks_capacity ? f->blocks_capacity * 2 : 128;
        f->blocks = realloc(f->blocks, sizeof(BlockInfo) * f->blocks_capacity);
    }
    Block block = f->blocks_size++;
    assert(block); // always non-zero
    BlockInfo *block_info = &f->blocks[block];
    memset(block_info, 0, sizeof(BlockInfo));
    block_info->idom_block = idom_block;
    block_info->params = IRREF_NONE;
    block_info->entry = IRREF_NONE;
    block_info->jumps[0] = IRREF_NONE;
    block_info->jumps[1] = IRREF_NONE;
    return block;
}

static IRRef get_static_value(Func *f, Type type, StaticValue static_value) {
    assert(type != TYPE_NONE);
    if (type == TYPE_BOOL) {
        return static_value.b ? IRREF_TRUE : IRREF_FALSE;
    }
    for (uint32_t salt = 0; salt < 10000; ++salt) {
        uint32_t hash = FNV_SEED;
        hash = fnv1a(&type, sizeof(type), hash);
        hash = fnv1a(&static_value, sizeof(static_value), hash);
        hash = fnv1a(&salt, sizeof(salt), hash);

        IRRef found;
        if (!HashedU32ToI32_get(&f->instr_to_result, hash, &found)) {
            if (f->static_value_size >= f->static_value_capacity) {
                grow_buffers(f, f->static_value_capacity * 2, f->instrs_capacity);
            }
            IRRef result = -(++f->static_value_size);
            assert(result); // always non-zero
            f->types[result] = type;
            f->static_values[result] = static_value;
            HashedU32ToI32_put(&f->instr_to_result, hash, result);
            return result;
        }
        
        assert(IRREF_IS_STATIC(found));
        if (f->types[found] == type && f->static_values[found].u == static_value.u) {
            return found;
        }
    }
    assert(0 && "should not happen");
    return IRREF_NONE;
}

static IRRef emit_instr(Func *f, OpCode opcode, int32_t left, int32_t right, Type type) {
    assert(opcode <= INSTR_OPCODE_MAX);
    assert(left <= INSTR_OPERAND_MAX);
    assert(right <= INSTR_OPERAND_MAX);
    assert(opcode == OP_PARAM || !f->blocks[f->curr_block].is_filled);

    Instr instr = (Instr) {
        .opcode = opcode,
        .left = left, 
        .right = right,
    };
    IRRef result = f->instrs_size;

    if (is_result_reusable(opcode)) {
        uint64_t visible_result;
        if (U64ToU64_get(&f->visible_instr_map, instr.u64_repr, &visible_result)) {
            return (IRRef) visible_result;
        }

        U64ToU64_put(&f->visible_instr_map, instr.u64_repr, result);

        if (f->visible_instr_stack_size >= f->visible_instr_stack_capacity) {
            f->visible_instr_stack_capacity = f->visible_instr_stack_capacity ? f->visible_instr_stack_capacity * 2 : 128;
            f->visible_instr_stack = realloc(f->visible_instr_stack, sizeof(VisibleInstr) * f->visible_instr_stack_capacity);
        }
        f->visible_instr_stack[f->visible_instr_stack_size++] = (VisibleInstr) {
            .block = f->curr_block,
            .instr = instr,
        };
    }

    if (result >= f->instrs_capacity) {
        grow_buffers(f, f->static_value_capacity, f->instrs_capacity * 2);
    }
    f->instrs[result] = instr;
    f->types[result] = type;
    ++f->instrs_size;

    print_instr(f, f->curr_block, result);
    return result;
}

void emit_block(Func *f, Block block) {
    // pop visible instructions that have been defined in blocks that are not our immediate dominator
    BlockInfo *block_info = &f->blocks[block];
    while (f->visible_instr_stack_size > 0 &&
           f->visible_instr_stack[f->visible_instr_stack_size - 1].block != block_info->idom_block)
    {
        U64ToU64_remove(&f->visible_instr_map, f->visible_instr_stack[--f->visible_instr_stack_size].instr.u64_repr);
    }

    if (f->entry_block == BLOCK_NONE) {
        f->entry_block = block;
    }

    block_info->entry = f->instrs_size;
    f->curr_block = block;
    emit_instr(f, OP_BLOCK, 0, block, TYPE_NONE);
}

void emit_sloc(Func *f, int32_t row, int32_t col) {
    emit_instr(f, OP_SLOC, row, col, TYPE_NONE);
}

IRRef get_static_type(Func *f, Type v)     { return get_static_value(f, TYPE_TYPE, (StaticValue) { .t = v }); }
IRRef get_static_bool(Func *f, bool v)     { return get_static_value(f, TYPE_BOOL, (StaticValue) { .b = v }); }
IRRef get_static_i8  (Func *f, int8_t v)   { return get_static_value(f, TYPE_I8,   (StaticValue) { .i = v }); }
IRRef get_static_i16 (Func *f, int16_t v)  { return get_static_value(f, TYPE_I16,  (StaticValue) { .i = v }); }
IRRef get_static_i32 (Func *f, int32_t v)  { return get_static_value(f, TYPE_I32,  (StaticValue) { .i = v }); }
IRRef get_static_i64 (Func *f, int64_t v)  { return get_static_value(f, TYPE_I64,  (StaticValue) { .i = v }); }
IRRef get_static_u8  (Func *f, uint8_t v)  { return get_static_value(f, TYPE_U8,   (StaticValue) { .u = v }); }
IRRef get_static_u16 (Func *f, uint16_t v) { return get_static_value(f, TYPE_U16,  (StaticValue) { .u = v }); }
IRRef get_static_u32 (Func *f, uint32_t v) { return get_static_value(f, TYPE_U32,  (StaticValue) { .u = v }); }
IRRef get_static_u64 (Func *f, uint64_t v) { return get_static_value(f, TYPE_U64,  (StaticValue) { .u = v }); }
IRRef get_static_f32 (Func *f, float v)    { return get_static_value(f, TYPE_F32,  (StaticValue) { .f = v }); }
IRRef get_static_f64 (Func *f, double v)   { return get_static_value(f, TYPE_F64,  (StaticValue) { .f = v }); }

IRRef emit_pair(Func *f, IRRef left, IRRef right) {
    return emit_instr(f, OP_PAIR, left, right, TYPE_PAIR);
}
static IRRef emit_create_ref_list(Func *f, int count, const IRRef *refs) {
    assert(count > 0);
    if (count == 1) {
        return refs[0];
    }
    IRRef result = emit_pair(f, refs[count - 2], refs[count - 1]);
    for (int i = count - 2; i > 0; --i) {
        result = emit_pair(f, refs[i - 1], result);
    }
    return result;
}
static int flatten_ref_list(Func *f, IRRef ref_list, IRRef *refs, int max_refs) {
    if (f->types[ref_list] != TYPE_PAIR) {
        refs[0] = ref_list;
        return 1;
    }
    int count = 0;
    IRRef ref = ref_list;
    for (;;) {
        Instr instr = f->instrs[ref];
        assert(instr.opcode == OP_PAIR);
        assert(f->types[instr.left] != TYPE_PAIR);
        assert(count + 1 <= max_refs);
        refs[count++] = instr.left;
        if (f->types[instr.right] != TYPE_PAIR) {
            assert(count + 1 <= max_refs);
            refs[count++] = instr.right;
            return count;
        }
        ref = instr.right;
    }
}
static int flatten_instr_args(Func *f, Instr instr, IRRef *refs, int max_refs) {
    int count = 0;
    const OpCodeInfo *opinfo = &opcode_info[instr.opcode];
    if (opinfo->left == OPERAND_REF) {
        Type type = f->types[instr.left];
        if (type == TYPE_PAIR) {
            count += flatten_ref_list(f, instr.left, refs, max_refs);
        } else {
            refs[count++] = instr.left;
        }
    }
    if (opinfo->right == OPERAND_REF) {
        Type type = f->types[instr.right];
        if (type == TYPE_PAIR) {
            count += flatten_ref_list(f, instr.right, refs + count, max_refs - count);
        } else {
            refs[count++] = instr.right;
        }
    }
    return count;
}


static int get_jump_slot(BlockInfo *from_block_info, Block to_block) {
    if (from_block_info->targets[0] == to_block) {
        return 0;
    }
    assert(from_block_info->targets[1] == to_block);
    return 1;
}
static int get_jump_arg_count(Func *f, BlockInfo *from_block_info, int jump_slot) {
    int count = 0;
    for (BlockArg args = from_block_info->args[jump_slot]; args; args = f->args[args].next) {
        ++count;
    }
    return count;
}
static void add_jump_arg(Func *f, BlockInfo *from_block_info, int jump_slot, IRRef ref) {
    if (f->args_size >= f->args_capacity) {
        f->args_capacity = f->args_capacity ? f->args_capacity * 2 : 128;
        f->args = realloc(f->args, sizeof(BlockArgEntry) * f->args_capacity);
    }
    BlockArg arg = ++f->args_size;
    f->args[arg].ref = ref;
    f->args[arg].next = from_block_info->args[jump_slot];
    from_block_info->args[jump_slot] = arg;
}
static IRRef get_jump_arg(Func *f, BlockInfo *from_block_info, int jump_slot, int index) {
    BlockArg arg = from_block_info->args[jump_slot];
    for (; index > 0; --index) {
        arg = f->args[arg].next;
    }
    assert(f->args[arg].ref != IRREF_NONE);
    return f->args[arg].ref;
}
static void remove_jump_arg(Func *f, BlockInfo *from_block_info, int jump_slot, int index) {
    BlockArg prev_arg = 0;
    BlockArg arg = from_block_info->args[jump_slot];
    assert(arg);
    for (; index > 0; --index) {
        prev_arg = arg;
        arg = f->args[arg].next;
        assert(arg);
    }
    if (prev_arg) {
        f->args[prev_arg].next = f->args[arg].next;
    } else {
        assert(from_block_info->args[jump_slot] == arg);
        from_block_info->args[jump_slot] = f->args[arg].next;
    }
}
static int get_param_count(Func *f, BlockInfo *block_info) {
    int count = 0;
    IRRef param = block_info->params;
    while (param != IRREF_NONE) {
        Instr param_instr = f->instrs[param];
        assert(param_instr.opcode == OP_PARAM);
        assert(param_instr.left != IRREF_NONE);
        ++count;
        param = param_instr.right;
    }
    return count;
}
static Symbol get_param_symbol(Func *f, BlockInfo *block_info, int index) {
    IRRef param = block_info->params;
    Instr param_instr;
    do {
        param_instr = f->instrs[param];
        assert(param_instr.opcode == OP_PARAM);
        assert(param_instr.left != IRREF_NONE);
        param = param_instr.right;
    } while (index-- > 0);
    return param_instr.left;
}
void emit_jump(Func *f, Block target) {
    assert(f->curr_block != BLOCK_NONE);
    BlockInfo *curr_block_info = &f->blocks[f->curr_block];
    if (curr_block_info->is_filled) {
        // if JUMP follows a JFALSE that specialized into a JUMP, then we'll already be finished
        return;
    }

    BlockInfo *target_block_info = &f->blocks[target];
    assert(target != curr_block_info->targets[1]);
    curr_block_info->jumps[0] = f->instrs_size;
    curr_block_info->targets[0] = target;
    curr_block_info->next_source[0] = target_block_info->sources[0];
    target_block_info->sources[0] = f->curr_block;
    ++target_block_info->source_count;

    emit_instr(f, OP_JUMP, 0, target, TYPE_NONE);
    curr_block_info->is_filled = true;
    
    int param_count = get_param_count(f, target_block_info);
    for (int i = param_count - 1; i >= 0; --i) {
        Symbol sym = get_param_symbol(f, target_block_info, i);
        IRRef ref = lookup_symbol(f, f->curr_block, sym);
        add_jump_arg(f, curr_block_info, 0, ref);
    }
}
void emit_jfalse(Func *f, IRRef cond, Block target) {
    assert(f->curr_block != BLOCK_NONE);
    assert(f->types[cond] == TYPE_BOOL);
    if (IRREF_IS_STATIC(cond)) {
        if (f->static_values[cond].b) {
            // no jump, so skip
        } else {
            emit_jump(f, target); // unconditional jump, since condition is false
        }
    } else {
        BlockInfo *curr_block_info = &f->blocks[f->curr_block];
        BlockInfo *target_block_info = &f->blocks[target];
        curr_block_info->jumps[1] = f->instrs_size;
        curr_block_info->targets[1] = target;
        curr_block_info->next_source[1] = target_block_info->sources[1];
        target_block_info->sources[1] = f->curr_block;
        ++target_block_info->source_count;
        emit_instr(f, OP_JFALSE, cond, target, TYPE_NONE);
    
        int param_count = get_param_count(f, target_block_info);
        for (int i = param_count - 1; i >= 0; --i) {
            Symbol sym = get_param_symbol(f, target_block_info, i);
            IRRef ref = lookup_symbol(f, f->curr_block, sym);
            add_jump_arg(f, curr_block_info, 1, ref);
        }
    }
}

IRRef emit_select(Func *f, IRRef cond, IRRef if_true, IRRef if_false) {
    assert(f->types[cond] == TYPE_BOOL);
    Type type = f->types[if_true];
    assert(type == f->types[if_false]);
    if (IRREF_IS_STATIC(cond)) {
        return f->static_values[cond].b ? if_true : if_false;
    }
    return emit_instr(f, OP_SELECT, cond, emit_pair(f, if_true, if_false), type);
}

void emit_ret(Func *f, IRRef ref) {
    BlockInfo *curr_block_info = &f->blocks[f->curr_block];
    if (curr_block_info->is_filled) {
        // if RET follows a JFALSE that specialized into a JUMP, then we'll already be finished
        return;
    }
    emit_instr(f, OP_RET, ref, 0, TYPE_NONE);
    curr_block_info->is_filled = true;
}



static void perform_static_bool_unop(UnaryOp unop, StaticValue *value, StaticValue *result) {
    switch (unop) {
    case UNOP_NOT: result->b = !value->b; break;
    default: assert(0 && "unary operator not implemented for int"); break;
    }
}
static void perform_static_int_unop(UnaryOp unop, StaticValue *value, StaticValue *result) {
    switch (unop) {
    case UNOP_NEG: result->i = -value->i; break;
    case UNOP_BNOT: result->i = ~value->i; break;
    default: assert(0 && "unary operator not implemented for int"); break;
    }
}
static void perform_static_uint_unop(UnaryOp unop, StaticValue *value, StaticValue *result) {
    switch (unop) {
    case UNOP_BNOT: result->u = ~value->u; break;
    default: assert(0 && "unary operator not implemented for int"); break;
    }
}
static void perform_static_real_unop(UnaryOp unop, StaticValue *value, StaticValue *result) {
    switch (unop) {
    case UNOP_NEG: result->f = -value->f; break;
    default: assert(0 && "unary operator not implemented for int"); break;
    }
}
static Type get_unop_result_type(UnaryOp unop, Type arg) {
    if (unop == UNOP_NEG) { assert(TYPE_IS_NUM(arg)); return arg; }
    if (unop == UNOP_NOT) { assert(arg == TYPE_BOOL); return arg; }
    if (unop == UNOP_BNOT) { assert(TYPE_IS_INT(arg)); return arg; }
    assert(0 && "illegal unop");
    return TYPE_NONE;
}
IRRef emit_unop(Func *f, UnaryOp unop, IRRef ref) {
    Type result_type = get_unop_result_type(unop, f->types[ref]);
    assert(result_type != TYPE_NONE);
    if (IRREF_IS_STATIC(ref)) {
        Type type = f->types[ref];
        const TypeInfo *type_info = get_type_info(type);
        StaticValue result = {0};
        switch (type_info->kind) {
        case TK_BOOL: perform_static_bool_unop(unop, &f->static_values[ref], &result); break;
        case TK_INT:  perform_static_int_unop(unop, &f->static_values[ref], &result); break;
        case TK_UINT: perform_static_uint_unop(unop, &f->static_values[ref], &result); break;
        case TK_REAL: perform_static_real_unop(unop, &f->static_values[ref], &result); break;
        default: assert(0 && "not implemented for type"); break;
        }
        return get_static_value(f, type, result);
    }
    return emit_instr(f, OP_UNOP, unop, ref, result_type);
}

static void perform_static_bool_binop(OpCode binop, StaticValue *left, StaticValue *right, StaticValue *result) {
    switch (binop) {
    case OP_AND: result->b = left->b && right->b; break;
    case OP_OR: result->b = left->b || right->b; break;
    case OP_EQ: result->b = left->b == right->b; break;
    case OP_NEQ: result->b = left->b != right->b; break;
    default: assert(0 && "binary operator not implemented for int"); break;
    }
}
static void perform_static_int_binop(OpCode binop, StaticValue *left, StaticValue *right, StaticValue *result) {
    switch (binop) {
    case OP_EQ: result->b = left->i == right->i; break;
    case OP_NEQ: result->b = left->i != right->i; break;
    case OP_LT: result->b = left->i < right->i; break;
    case OP_GT: result->b = left->i > right->i; break;
    case OP_LTEQ: result->b = left->i <= right->i; break;
    case OP_GTEQ: result->b = left->i >= right->i; break;
    case OP_ADD: result->i = left->i + right->i; break;
    case OP_SUB: result->i = left->i - right->i; break;
    case OP_MUL: result->i = left->i * right->i; break;
    case OP_DIV: result->i = left->i / right->i; break;
    case OP_MIN: result->i = MIN(left->i, right->i); break;
    case OP_MAX: result->i = MAX(left->i, right->i); break;
    case OP_BAND: result->i = left->i & right->i; break;
    case OP_BOR: result->i = left->i | right->i; break;
    case OP_BXOR: result->i = left->i ^ right->i; break;
    case OP_BSHL: result->i = left->i << right->i; break;
    case OP_BSHR: result->i = left->i >> right->i; break;
    default: assert(0 && "binary operator not implemented for int"); break;
    }
}
static void perform_static_uint_binop(OpCode binop, StaticValue *left, StaticValue *right, StaticValue *result) {
    switch (binop) {
    case OP_EQ: result->b = left->u == right->u; break;
    case OP_NEQ: result->b = left->u != right->u; break;
    case OP_LT: result->b = left->u < right->u; break;
    case OP_GT: result->b = left->u > right->u; break;
    case OP_LTEQ: result->b = left->u <= right->u; break;
    case OP_GTEQ: result->b = left->u >= right->u; break;
    case OP_ADD: result->u = left->u + right->u; break;
    case OP_SUB: result->u = left->u - right->u; break;
    case OP_MUL: result->u = left->u * right->u; break;
    case OP_DIV: result->u = left->u / right->u; break;
    case OP_MIN: result->u = MIN(left->u, right->u); break;
    case OP_MAX: result->u = MAX(left->u, right->u); break;
    case OP_BAND: result->u = left->u & right->u; break;
    case OP_BOR: result->u = left->u | right->u; break;
    case OP_BXOR: result->u = left->u ^ right->u; break;
    case OP_BSHL: result->u = left->u << right->u; break;
    case OP_BSHR: result->u = left->u >> right->u; break;
    default: assert(0 && "binary operator not implemented for int"); break;
    }
}
static void perform_static_real_binop(OpCode binop, StaticValue *left, StaticValue *right, StaticValue *result) {
    switch (binop) {
    case OP_EQ: result->b = left->f == right->f; break;
    case OP_NEQ: result->b = left->f != right->f; break;
    case OP_LT: result->b = left->f < right->f; break;
    case OP_GT: result->b = left->f > right->f; break;
    case OP_LTEQ: result->b = left->f <= right->f; break;
    case OP_GTEQ: result->b = left->f >= right->f; break;
    case OP_ADD: result->f = left->f + right->f; break;
    case OP_SUB: result->f = left->f - right->f; break;
    case OP_MUL: result->f = left->f * right->f; break;
    case OP_DIV: result->f = left->f / right->f; break;
    case OP_MIN: result->f = MIN(left->f, right->f); break;
    case OP_MAX: result->f = MAX(left->f, right->f); break;
    case OP_POW: result->f = pow(left->f, right->f); break;
    default: assert(0 && "binary operator not implemented for int"); break;
    }
}
static Type get_binop_result_type(OpCode binop, Type left, Type right) {
    assert(OP_IS_BINOP(binop));
    assert(left == right);
    if (OP_IS_CMP_BINOP(binop)) { return TYPE_BOOL; }
    if (OP_IS_BOOL_BINOP(binop)) { assert(left == TYPE_BOOL); return TYPE_BOOL; }
    if (OP_IS_INT_BINOP(binop)) { assert(TYPE_IS_INT(left)); return left; }
    if (OP_IS_NUM_BINOP(binop)) { assert(TYPE_IS_NUM(left)); return left; }
    assert(0 && "illegal binop");
    return TYPE_NONE;
}
IRRef emit_binop(Func *f, OpCode binop, IRRef left, IRRef right) {
    Type left_type = f->types[left];
    Type right_type = f->types[right];
    Type result_type = get_binop_result_type(binop, left_type, right_type);
    assert(result_type != TYPE_NONE);

    if (IRREF_IS_STATIC(left) && IRREF_IS_STATIC(right)) {
        StaticValue result = {0};
        switch (get_type_info(left_type)->kind) {
        case TK_BOOL: perform_static_bool_binop(binop, &f->static_values[left], &f->static_values[right], &result); break;
        case TK_INT:  perform_static_int_binop(binop, &f->static_values[left], &f->static_values[right], &result); break;
        case TK_UINT: perform_static_uint_binop(binop, &f->static_values[left], &f->static_values[right], &result); break;
        case TK_REAL: perform_static_real_binop(binop, &f->static_values[left], &f->static_values[right], &result); break;
        default: assert(0 && "not implemented for type"); break;
        }
        return get_static_value(f, result_type, result);
    }

    return emit_instr(f, binop, left, right, result_type);
}






// requires Func *f to be visible. don't count on 'break' exiting the loop completely
#define FOREACH_SOURCE(Var, B) \
    for (int _root = 0; _root < 2; ++_root) \
    for (Block Var = f->blocks[B].sources[_root]; Var != BLOCK_NONE; Var = f->blocks[Var].next_source[_root])

static int flatten_sources(Func *f, Block block, Block *output, int max_output) {
    int count = 0;
    FOREACH_SOURCE(source, block) {
        assert(count < max_output);
        output[count++] = source;
    }
    return count;
}




static int flatten_params(Func *f, IRRef param, IRRef *output, int max_output) {
    int count = 0;
    while (param != IRREF_NONE) {
        Instr param_instr = f->instrs[param];
        assert(param_instr.opcode == OP_PARAM);
        assert(count < max_output);
        assert(param_instr.left != IRREF_NONE);
        output[count++] = param_instr.left;
        param = param_instr.right;
    }
    return count;
}
static void remove_block_param(Func *f, Block block, int index) {

}

// check if all jumps to this block pass the same value, and if so remove both the param, and the branch arguments.
// if the branch arguments are arguments to that block, then apply this function recursively.
static void try_eliminate_param(Func *f, Block block, int param_index) {
    IRRef first_arg = IRREF_NONE;
    FOREACH_SOURCE(source_block, block) {
        BlockInfo *source_block_info = &f->blocks[source_block];
        int jump_slot = get_jump_slot(source_block_info, block);
        IRRef arg = get_jump_arg(f, source_block_info, jump_slot, param_index);
        if (first_arg == IRREF_NONE) {
            first_arg = arg;
        } else if (arg != first_arg) {
            return;
        }
    }
    remove_block_param(f, block, param_index);
    FOREACH_SOURCE(source_block, block) {
        BlockInfo *source_block_info = &f->blocks[source_block];
        int jump_slot = get_jump_slot(source_block_info, block);
        remove_jump_arg(f, source_block_info, jump_slot, param_index);
        //try_eliminate_param(f, source_block, )
    }
}

void define_symbol(Func *f, Block block, Symbol sym, IRRef ref) {
    assert(block != TYPE_NONE);
    assert(sym != SYMBOL_NONE);
    assert(ref != IRREF_NONE);
    EnvKey key = { .block = block, .sym = sym };
    EnvValue value = { .ref = ref };
    U64ToU64_put(&f->env_key_to_ref, key.u64_repr, value.u64_repr);
}

static IRRef add_param_internal(Func *f, Block block, Symbol sym, Type type) {
    assert(block != BLOCK_NONE);
    assert(sym != SYMBOL_NONE);
    BlockInfo *block_info = &f->blocks[block];
    IRRef param = emit_instr(f, OP_PARAM, sym, block_info->params, type);
    block_info->params = param;
    EnvKey key = { .block = block, .sym = sym };
    EnvValue value = { .ref = param };
    U64ToU64_put(&f->env_key_to_ref, key.u64_repr, value.u64_repr);
    return param;
}
IRRef add_param(Func *f, Block block, Symbol sym, Type type) {
    assert(type != TYPE_NONE);
    return add_param_internal(f, block, sym, type);
}

/*
static Symbol get_param_symbol(Func *f, Block block, int index) {

}
static void fixup_jumps_to_block(Func *f, Block block) {
    assert(block != BLOCK_NONE);
    BlockInfo *block_info = &f->blocks[block];
    int param_count = get_param_count(f, block_info);
    if (param_count == 0) {
        return;
    }

    FOREACH_SOURCE(source_block, block) {
        Instr *jump_instr = &f->instrs[find_jump(f, source_block, block)];
        int arg_count = get_arg_count(f, jump_instr->extra);
        assert(arg_count <= param_count);
        int missing_count = param_count - arg_count;

        for (int i = missing_count - 1; i <= 0; --i) {
            Symbol sym = get_param_symbol(f, block, i);
            IRRef ref = lookup_symbol(f, source_block, sym);
            jump_instr->extra = create_block_arg(f, ref, jump_instr->extra);
        }
    }
}
*/

void seal_block(Func *f, Block block) {
    assert(block != BLOCK_NONE);
    BlockInfo *block_info = &f->blocks[block];
    assert(!block_info->is_sealed);
    block_info->is_sealed = true;
    //fixup_jumps_to_block(f, block);
}

IRRef lookup_symbol(Func *f, Block block, Symbol sym) {
    EnvKey key = { .block = block, .sym = sym };
    EnvValue value;
    if (U64ToU64_get(&f->env_key_to_ref, key.u64_repr, &value.u64_repr)) {
        return value.ref;
    }

    // not found in this block. must recurse to source blocks.
    
    BlockInfo *block_info = &f->blocks[block];
    if (block_info->is_sealed) {
        // being sealed means there will not be more source blocks
        if (block_info->source_count == 0) {
            // not found, and this is the entry block. so it doesn't exist.
            return IRREF_NONE;
        }
        if (block_info->source_count == 1) {
            // trivial case
            FOREACH_SOURCE(source_block, block) {
                return lookup_symbol(f, source_block, sym);
            }
        }
    }

    int prev_param_count = get_param_count(f, block_info);

    // this is a merge block, so we need to get this value as an argument.
    // add incomplete parameter to avoid infinite recursion (it will get correct type later).
    IRRef param = add_param_internal(f, block, sym, TYPE_NONE);

    // we fixup the jumps in any blocks already known to target this.
    FOREACH_SOURCE(source_block, block) {
        IRRef ref = lookup_symbol(f, source_block, sym);
        assert(ref != IRREF_NONE);
        
        // disallow cycles, before there are any known definitions.
        // code generation must ensure definitions are added to the graph before generating uses.
        assert(ref != param);
        assert(f->types[ref] != TYPE_NONE);
        f->types[param] = f->types[ref]; // fixup type

        // add jump argument
        BlockInfo *source_block_info = &f->blocks[source_block];
        int jump_slot = get_jump_slot(source_block_info, block);
        assert(prev_param_count == get_jump_arg_count(f, source_block_info, jump_slot));
        add_jump_arg(f, source_block_info, jump_slot, ref);
    }

    //try_eliminate_param(f, block, 0);
    return param;
}






static void print_static_value(Func *f, IRRef ref) {
    assert(IRREF_IS_STATIC(ref));
    StaticValue *static_value = &f->static_values[ref];
    const TypeInfo *type_info = get_type_info(f->types[ref]);
    switch (type_info->kind) {
    case TK_UNIT:
        printf("()");
        break;
    case TK_BOOL:
        printf("<%s>", static_value->b ? "true" : "false");
        break;
    case TK_INT:
        printf("<%ld>", static_value->i);
        break;
    case TK_UINT:
        printf("<%lu>", static_value->u);
        break;
    case TK_REAL:
        printf("<%f>", static_value->f);
        break;
    default:
        printf("%d", ref);
        break;
    }
}
static void print_value_operand(Func *f, IRRef ref) {
    if (IRREF_IS_STATIC(ref)) {
        print_static_value(f, ref);
    } else {
        printf("%d", ref);
    }
}
static void print_operand(Func *f, OperandType operand_type, int32_t operand) {
    switch (operand_type) {
    case OPERAND_NONE:
        break;
    case OPERAND_REF:
        putchar(' ');
        print_value_operand(f, operand);
        break;
    case OPERAND_BLOCK:
        printf(" :%d", operand);
        break;
    case OPERAND_SYM:
        printf(" %s", symbol_name(operand));
        break;
    case OPERAND_UNOP:
        assert(0);
        break;
    case OPERAND_FUNC:
        printf(" @%d", operand);
        break;
    case OPERAND_ROW:
        printf(" row=%d", operand);
        break;
    case OPERAND_COL:
        printf(" col=%d", operand);
        break;
    }
}
static const char *get_type_suffix_for_type(Type type) {
    switch (type) {
    case TYPE_UNIT: return ".unit";
    case TYPE_BOOL: return ".bool";
    case TYPE_I8: return ".i8";
    case TYPE_I16: return ".i16";
    case TYPE_I32: return ".i32";
    case TYPE_I64: return ".i64";
    case TYPE_U8: return ".u8";
    case TYPE_U16: return ".u16";
    case TYPE_U32: return ".u32";
    case TYPE_U64: return ".u64";
    case TYPE_F32: return ".f32";
    case TYPE_F64: return ".f64";
    default: return "";
    }
}
static const char *get_type_suffix(Func *f, Instr instr) {
    if (instr.opcode == OP_PAIR || instr.opcode == OP_JFALSE) {
        return "";
    }
    const OpCodeInfo *opinfo = &opcode_info[instr.opcode];
    if (opinfo->left == OPERAND_REF) {
        return get_type_suffix_for_type(f->types[instr.left]);
    }
    if (opinfo->right == OPERAND_REF) {
        return get_type_suffix_for_type(f->types[instr.right]);
    }
    return "";
}
static const char *get_unop_name(UnaryOp unop) {
    switch (unop) {
    case UNOP_NEG: return "neg";
    case UNOP_NOT: return "not";
    case UNOP_BNOT: return "bnot";
    default: assert(0 && "illegal unop"); break;
    }
}
static void print_params(Func *f, IRRef param) {
    bool has_printed = false;
    while (param != IRREF_NONE) {
        if (has_printed) {
            printf(", ");
        } else {
            printf(" (");
        }
        has_printed = true;
        Instr instr = f->instrs[param];
        assert(instr.opcode == OP_PARAM);
        printf("%s/%d", symbol_name(instr.left), param);
        param = instr.right;
    }
    if (has_printed) {
        putchar(')');
    }
}
static void print_jump_args(Func *f, Block from_block, Block to_block) {
    BlockInfo *from_block_info = &f->blocks[from_block];
    int jump_slot = get_jump_slot(from_block_info, to_block);
    BlockArg arg = from_block_info->args[jump_slot];
    bool has_printed = false;
    while (arg) {
        if (has_printed) {
            printf(", ");
        } else {
            printf(" (");
        }
        has_printed = true;
        BlockArgEntry entry = f->args[arg];
        print_value_operand(f, entry.ref);
        arg = entry.next;
    }
    if (has_printed) {
        putchar(')');
    }
}
static void print_instr(Func *f, Block block, IRRef i) {
    Instr instr = f->instrs[i];
    const OpCodeInfo *opinfo = &opcode_info[instr.opcode];
    if (instr.opcode == OP_NOP) {
        // don't print
    } else if (instr.opcode == OP_PARAM) {
        // don't print since they are emitted all over the place, and don't do anything.
        // they are only used for storage (to get an IRRef)
    } else if (instr.opcode == OP_BLOCK) {
        printf(":%d", instr.right);
        print_params(f, f->blocks[instr.right].params);
        putchar('\n');
    } else if (instr.opcode == OP_UNOP) {
        printf("    %d <- %s%s ", i, get_unop_name((UnaryOp)instr.left), get_type_suffix(f, instr));
        print_value_operand(f, instr.right);
        putchar('\n');
    } else {
        if (OP_HAS_RESULT(instr.opcode)) {
            printf("    %d <- %s%s", i, opinfo->name, get_type_suffix(f, instr));
        } else {
            printf("    %s%s", opinfo->name, get_type_suffix(f, instr));
        }
        print_operand(f, opinfo->left, instr.left);
        print_operand(f, opinfo->right, instr.right);
        if (instr.opcode == OP_JUMP || instr.opcode == OP_JFALSE) {
            print_jump_args(f, block, instr.right);
        }
        putchar('\n');
    }
}
void print_code(Func *f) {
    Block curr_block = BLOCK_NONE;
    for (int i = 0; i < f->instrs_size; ++i) {
        Instr instr = f->instrs[i];
        if (instr.opcode == OP_BLOCK) {
            curr_block = instr.right;
        }
        print_instr(f, curr_block, i);
    }
}
