#include "func.h"
#include "fnv.h"
#include "vector.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

static void print_instr(Func *f, Block block, Instr instr);

typedef union EnvKey {
    struct {
        Block block;
        Symbol sym;
    };
    uint64_t u64_repr;
} EnvKey;

typedef union EnvValue {
    Value ref;
    uint64_t u64_repr;
} EnvValue;


typedef union StaticValue {
    Type t;
    bool b;
    int64_t i;
    uint64_t u;
    double f;
} StaticValue;

typedef struct BlockParam {
    Symbol name;
    Value value;
} BlockParam;

typedef struct BlockInfo {
    Block targets[2]; // jump target is targets[0], and jfalse target (if any) is targets[1].
    Value *args[2]; // argument (vectors) to the corresponding jump instructions

    Block *sources; // vector
    BlockParam *params; // (vector)
    Instr *instrs; // vector

    bool is_filled : 1;
    bool is_sealed : 1;
} BlockInfo;


struct Func {
    StaticValue *static_values;
    Type *value_types;
    int static_value_size;
    int static_value_capacity;
    int other_value_size;
    int other_value_capacity;

    BlockInfo *blocks;
    int blocks_size;
    int blocks_capacity;
    Block curr_block;

    HashedU32ToI32 static_value_hash_to_value;

    U64ToU64 env_key_to_ref;
};


void set_current_block(Func *f, Block block) {
    f->curr_block = block;
    printf(":%d\n", block);
}

Block get_current_block(Func *f) {
    return f->curr_block;
}

Func *func_new() {
    Func *f = calloc(1, sizeof(Func));
    f->blocks_size = 1; // reserve slot for BLOCK_NONE
    f->blocks_capacity = 128;
    f->blocks = calloc(1, sizeof(BlockInfo) * f->blocks_capacity);
    
    f->static_value_capacity = 128;
    f->other_value_capacity = 128;
    int total_value_capacity = f->static_value_capacity + f->other_value_capacity;
    f->static_values = (StaticValue *)calloc(1, sizeof(StaticValue) * total_value_capacity) + f->static_value_capacity;
    f->value_types = (Type *)calloc(1, sizeof(Type) * total_value_capacity) + f->static_value_capacity;

    f->value_types[VALUE_NONE] = TYPE_NONE;
    f->value_types[VALUE_UNIT] = TYPE_UNIT;
    f->value_types[VALUE_FALSE] = TYPE_BOOL;
    f->value_types[VALUE_TRUE] = TYPE_BOOL;
    f->static_values[VALUE_TRUE].b = true;
    f->static_value_size = 4; // UNIT/FALSE/TRUE
    f->other_value_size = 1; // NONE
    return f;
}

static void grow_value_buffers(Func *f, int static_value_capacity, int other_value_capacity) {
    int total_capacity = static_value_capacity + other_value_capacity;
    int static_size = f->static_value_size;
    int total_size = static_size + f->other_value_size;

    StaticValue *static_values = (StaticValue *)calloc(1, sizeof(StaticValue) * total_capacity) + static_value_capacity;
    Type *value_types = (Type *)calloc(1, sizeof(Type) * total_capacity) + static_value_capacity;

    memcpy(static_values - static_size, f->static_values - static_size, total_size);
    memcpy(value_types - static_size, f->value_types - static_size, total_size);

    free(f->static_values - f->static_value_capacity);
    free(f->value_types - f->static_value_capacity);

    f->static_values = static_values;
    f->value_types = value_types;
    f->static_value_capacity = static_value_capacity;
    f->other_value_capacity = other_value_capacity;
}

Block create_block(Func *f) {
    if (f->blocks_size >= f->blocks_capacity) {
        f->blocks_capacity = f->blocks_capacity ? f->blocks_capacity * 2 : 128;
        f->blocks = realloc(f->blocks, sizeof(BlockInfo) * f->blocks_capacity);
    }
    Block block = f->blocks_size++;
    assert(block); // always non-zero
    BlockInfo *block_info = &f->blocks[block];
    memset(block_info, 0, sizeof(BlockInfo));
    return block;
}

static Value get_static_value(Func *f, Type type, StaticValue static_value) {
    assert(type != TYPE_NONE);
    if (type == TYPE_BOOL) {
        return static_value.b ? VALUE_TRUE : VALUE_FALSE;
    }
    for (uint32_t salt = 0; salt < 10000; ++salt) {
        uint32_t hash = FNV_SEED;
        hash = fnv1a(&type, sizeof(type), hash);
        hash = fnv1a(&static_value, sizeof(static_value), hash);
        hash = fnv1a(&salt, sizeof(salt), hash);

        Value found;
        if (!HashedU32ToI32_get(&f->static_value_hash_to_value, hash, &found)) {
            if (f->static_value_size >= f->static_value_capacity) {
                grow_value_buffers(f, f->static_value_capacity * 2, f->other_value_capacity);
            }
            Value result = -(++f->static_value_size);
            assert(result); // always non-zero
            f->value_types[result] = type;
            f->static_values[result] = static_value;
            HashedU32ToI32_put(&f->static_value_hash_to_value, hash, result);
            return result;
        }
        
        assert(VALUE_IS_STATIC(found));
        if (f->value_types[found] == type && f->static_values[found].u == static_value.u) {
            return found;
        }
    }
    assert(0 && "should not happen");
    return VALUE_NONE;
}

static Value create_value(Func *f, Type type) {
    if (f->other_value_size >= f->other_value_capacity) {
        grow_value_buffers(f, f->static_value_capacity, f->other_value_capacity * 2);
    }
    Value result = f->other_value_size++;
    assert(result <= INSTR_OPERAND_MAX);
    f->value_types[result] = type;
    return result;
}

static Value emit_instr(Func *f, OpCode opcode, int32_t left, int32_t right, Type type) {
    BlockInfo *curr_block_info = &f->blocks[f->curr_block];
    assert(opcode <= INSTR_OPCODE_MAX);
    assert(left <= INSTR_OPERAND_MAX);
    assert(right <= INSTR_OPERAND_MAX);
    assert(!curr_block_info->is_filled);
    assert(type != TYPE_NONE || !OP_HAS_RESULT(opcode));

    Value result = create_value(f, type);
    Instr instr = (Instr) {
        .opcode = opcode,
        .left = left, 
        .right = right,
        .result = result,
    };
    vector_push(curr_block_info->instrs, instr);
    print_instr(f, f->curr_block, instr);
    return result;
}

Value get_static_type(Func *f, Type v)     { return get_static_value(f, TYPE_TYPE, (StaticValue) { .t = v }); }
Value get_static_bool(Func *f, bool v)     { return get_static_value(f, TYPE_BOOL, (StaticValue) { .b = v }); }
Value get_static_i8  (Func *f, int8_t v)   { return get_static_value(f, TYPE_I8,   (StaticValue) { .i = v }); }
Value get_static_i16 (Func *f, int16_t v)  { return get_static_value(f, TYPE_I16,  (StaticValue) { .i = v }); }
Value get_static_i32 (Func *f, int32_t v)  { return get_static_value(f, TYPE_I32,  (StaticValue) { .i = v }); }
Value get_static_i64 (Func *f, int64_t v)  { return get_static_value(f, TYPE_I64,  (StaticValue) { .i = v }); }
Value get_static_u8  (Func *f, uint8_t v)  { return get_static_value(f, TYPE_U8,   (StaticValue) { .u = v }); }
Value get_static_u16 (Func *f, uint16_t v) { return get_static_value(f, TYPE_U16,  (StaticValue) { .u = v }); }
Value get_static_u32 (Func *f, uint32_t v) { return get_static_value(f, TYPE_U32,  (StaticValue) { .u = v }); }
Value get_static_u64 (Func *f, uint64_t v) { return get_static_value(f, TYPE_U64,  (StaticValue) { .u = v }); }
Value get_static_f32 (Func *f, float v)    { return get_static_value(f, TYPE_F32,  (StaticValue) { .f = v }); }
Value get_static_f64 (Func *f, double v)   { return get_static_value(f, TYPE_F64,  (StaticValue) { .f = v }); }

Value emit_pair(Func *f, Value left, Value right) {
    return emit_instr(f, OP_PAIR, left, right, TYPE_PAIR);
}
static Value emit_create_ref_list(Func *f, int count, const Value *refs) {
    assert(count > 0);
    if (count == 1) {
        return refs[0];
    }
    Value result = emit_pair(f, refs[count - 2], refs[count - 1]);
    for (int i = count - 2; i > 0; --i) {
        result = emit_pair(f, refs[i - 1], result);
    }
    return result;
}

static int get_jump_slot(BlockInfo *from_block_info, Block to_block) {
    if (from_block_info->targets[0] == to_block) {
        return 0;
    }
    assert(from_block_info->targets[1] == to_block);
    return 1;
}
static int get_explicit_param_count(BlockInfo *block_info) {
    int count = 0;
    BlockParam p;
    vector_foreach(block_info->params, p) {
        if (p.name == SYMBOL_NONE) {
            ++count;
        }
    }
    return count;
}
static void add_jump_args(Func *f, BlockInfo *target_block_info, BlockInfo *source_block_info, int jump_slot, int arg_count, Value *args) {
    assert(arg_count == get_explicit_param_count(target_block_info));
    for (int i = 0; i < arg_count; ++i) {
        assert(args[i] != VALUE_NONE);
        vector_push(source_block_info->args[jump_slot], args[i]);
    }
    for (int i = arg_count; i < vector_size(target_block_info->params); ++i) {
        Value value = lookup_symbol(f, f->curr_block, target_block_info->params[i].name);
        vector_push(source_block_info->args[jump_slot], value);
    }
}
void emit_jump_with_args(Func *f, Block target, int arg_count, Value *args) {
    assert(f->curr_block != BLOCK_NONE);
    BlockInfo *curr_block_info = &f->blocks[f->curr_block];
    BlockInfo *target_block_info = &f->blocks[target];
    if (curr_block_info->is_filled) {
        // if JUMP follows a JFALSE that specialized into a JUMP, then we'll already be finished
        return;
    }
    assert(target != curr_block_info->targets[1]);
    curr_block_info->targets[0] = target;
    vector_push(target_block_info->sources, f->curr_block);
    emit_instr(f, OP_JUMP, 0, target, TYPE_NONE);
    curr_block_info->is_filled = true;
    add_jump_args(f, target_block_info, curr_block_info, 0, arg_count, args);
}
void emit_jfalse_with_args(Func *f, Value cond, Block target, int arg_count, Value *args) {
    assert(f->curr_block != BLOCK_NONE);
    assert(f->value_types[cond] == TYPE_BOOL);
    if (VALUE_IS_STATIC(cond)) {
        if (f->static_values[cond].b) {
            // no jump, so skip
        } else {
            emit_jump(f, target); // unconditional jump, since condition is false
        }
    } else {
        BlockInfo *curr_block_info = &f->blocks[f->curr_block];
        BlockInfo *target_block_info = &f->blocks[target];
        curr_block_info->targets[1] = target;
        vector_push(target_block_info->sources, f->curr_block);
        emit_instr(f, OP_JFALSE, cond, target, TYPE_NONE);
        add_jump_args(f, target_block_info, curr_block_info, 1, arg_count, args);
    }
}
void emit_jump(Func *f, Block target) {
    emit_jump_with_args(f, target, 0, NULL);
}
void emit_jfalse(Func *f, Value cond, Block target) {
    emit_jfalse_with_args(f, cond, target, 0, NULL);
}

Value emit_select(Func *f, Value cond, Value if_true, Value if_false) {
    assert(f->value_types[cond] == TYPE_BOOL);
    Type type = f->value_types[if_true];
    assert(type == f->value_types[if_false]);
    if (VALUE_IS_STATIC(cond)) {
        return f->static_values[cond].b ? if_true : if_false;
    }
    return emit_instr(f, OP_SELECT, cond, emit_pair(f, if_true, if_false), type);
}

void emit_ret(Func *f, Value ref) {
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
Value emit_unop(Func *f, UnaryOp unop, Value ref) {
    Type result_type = get_unop_result_type(unop, f->value_types[ref]);
    assert(result_type != TYPE_NONE);
    if (VALUE_IS_STATIC(ref)) {
        Type type = f->value_types[ref];
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
Value emit_binop(Func *f, OpCode binop, Value left, Value right) {
    Type left_type = f->value_types[left];
    Type right_type = f->value_types[right];
    Type result_type = get_binop_result_type(binop, left_type, right_type);
    assert(result_type != TYPE_NONE);

    if (VALUE_IS_STATIC(left) && VALUE_IS_STATIC(right)) {
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




static void remove_block_param(Func *f, Block block, int index) {

}

// check if all jumps to this block pass the same value, and if so remove both the param, and the branch arguments.
// if the branch arguments are arguments to that block, then apply this function recursively.
static void try_eliminate_param(Func *f, Block block, int param_index) {
    Value first_arg = VALUE_NONE;
    BlockInfo *block_info = &f->blocks[block];
    Block source_block;
    vector_foreach(block_info->sources, source_block) {
        BlockInfo *source_block_info = &f->blocks[source_block];
        int jump_slot = get_jump_slot(source_block_info, block);
        Value arg = source_block_info->args[jump_slot][param_index];
        if (first_arg == VALUE_NONE) {
            first_arg = arg;
        } else if (arg != first_arg) {
            return;
        }
    }
    remove_block_param(f, block, param_index);
    vector_foreach(block_info->sources, source_block) {
        BlockInfo *source_block_info = &f->blocks[source_block];
        int jump_slot = get_jump_slot(source_block_info, block);
        vector_erase(source_block_info->args[jump_slot], param_index);
        //try_eliminate_param(f, source_block, )
    }
}

void define_symbol(Func *f, Block block, Symbol sym, Value ref) {
    assert(block != TYPE_NONE);
    assert(sym != SYMBOL_NONE);
    assert(ref != VALUE_NONE);
    EnvKey key = { .block = block, .sym = sym };
    EnvValue value = { .ref = ref };
    U64ToU64_put(&f->env_key_to_ref, key.u64_repr, value.u64_repr);
}

static Value add_param_internal(Func *f, Block block, Symbol name, Type type) {
    assert(block != BLOCK_NONE);
    BlockInfo *block_info = &f->blocks[block];
    if (name == SYMBOL_NONE) { // explicit parameter
        BlockParam p;
        vector_foreach(block_info->params, p) {
            assert(p.name == SYMBOL_NONE && "explicit params must be added first (using add_block_param)");
        }
    }
    Value value = create_value(f, type);
    vector_push(block_info->params, ((BlockParam) {
        .name = name,
        .value = value,
    }));
    return value;
}
Value add_block_param(Func *f, Block block, Type type) {
    assert(type != TYPE_NONE);
    return add_param_internal(f, block, SYMBOL_NONE, type);
}

/*
static void fixup_jumps_to_block(Func *f, Block block) {
    assert(block != BLOCK_NONE);
    BlockInfo *block_info = &f->blocks[block];
    int param_count = get_param_count(block_info);
    if (param_count == 0) {
        return;
    }

    FOREACH_SOURCE(source_block, block) {
        Instr *jump_instr = &f->instrs[find_jump(f, source_block, block)];
        int arg_count = get_arg_count(f, jump_instr->extra);
        assert(arg_count <= param_count);
        int missing_count = param_count - arg_count;

        for (int i = missing_count - 1; i <= 0; --i) {
            Symbol sym = get_param_symbol(block, i);
            Value ref = lookup_symbol(f, source_block, sym);
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

Value lookup_symbol(Func *f, Block block, Symbol sym) {
    assert(block != SYMBOL_NONE);
    assert(sym != SYMBOL_NONE);
    EnvKey key = { .block = block, .sym = sym };
    EnvValue value;
    if (U64ToU64_get(&f->env_key_to_ref, key.u64_repr, &value.u64_repr)) {
        return value.ref;
    }

    // not found in this block. must recurse to source blocks.
    
    BlockInfo *block_info = &f->blocks[block];
    if (block_info->is_sealed) {
        // being sealed means there will not be more source blocks
        if (vector_empty(block_info->sources)) {
            // not found, and this is the entry block. so it doesn't exist.
            return VALUE_NONE;
        }
        if (vector_size(block_info->sources) == 1) {
            // trivial case
            return lookup_symbol(f, block_info->sources[0], sym);
        }
    }

    int prev_param_count = vector_size(block_info->params);

    // this is a merge block, so we need to get this value as an argument.
    // add incomplete parameter to avoid infinite recursion (it will get correct type later).
    Value param = add_param_internal(f, block, sym, TYPE_NONE);

    // we fixup the jumps in any blocks already known to target this.
    Block source_block;
    vector_foreach(block_info->sources, source_block) {
        Value ref = lookup_symbol(f, source_block, sym);
        assert(ref != VALUE_NONE);
        
        // disallow cycles, before there are any known definitions.
        // code generation must ensure definitions are added to the graph before generating uses.
        assert(ref != param);
        assert(f->value_types[ref] != TYPE_NONE);
        f->value_types[param] = f->value_types[ref]; // fixup type

        // add jump argument
        BlockInfo *source_block_info = &f->blocks[source_block];
        int jump_slot = get_jump_slot(source_block_info, block);
        assert(prev_param_count == vector_size(source_block_info->args[jump_slot]));
        vector_push(source_block_info->args[jump_slot], ref);
    }

    //try_eliminate_param(f, block, 0);
    return param;
}






static void print_static_value(Func *f, Value ref) {
    assert(VALUE_IS_STATIC(ref));
    StaticValue *static_value = &f->static_values[ref];
    const TypeInfo *type_info = get_type_info(f->value_types[ref]);
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
static void print_value_operand(Func *f, Value value) {
    if (VALUE_IS_STATIC(value)) {
        print_static_value(f, value);
    } else {
        printf("%d", value);
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
    case OPERAND_UNOP:
        assert(0);
        break;
    case OPERAND_FUNC:
        printf(" @%d", operand);
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
        return get_type_suffix_for_type(f->value_types[instr.left]);
    }
    if (opinfo->right == OPERAND_REF) {
        return get_type_suffix_for_type(f->value_types[instr.right]);
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
static void print_params(Func *f, Block block) {
    BlockInfo *block_info = &f->blocks[block];
    bool has_printed = false;
    for (int i = 0; i < vector_size(block_info->params); ++i) {
        if (has_printed) {
            printf(", ");
        } else {
            printf(" (");
        }
        has_printed = true;
        printf("%d", block_info->params[i].value);
    }
    if (has_printed) {
        putchar(')');
    }
}
static void print_jump_args(Func *f, Block from_block, Block to_block) {
    BlockInfo *from_block_info = &f->blocks[from_block];
    int jump_slot = get_jump_slot(from_block_info, to_block);
    bool has_printed = false;
    for (int i = 0; i < vector_size(from_block_info->args[jump_slot]); ++i) {
        if (has_printed) {
            printf(", ");
        } else {
            printf(" (");
        }
        has_printed = true;
        print_value_operand(f, from_block_info->args[jump_slot][i]);
    }
    if (has_printed) {
        putchar(')');
    }
}
static void print_instr(Func *f, Block block, Instr instr) {
    const OpCodeInfo *opinfo = &opcode_info[instr.opcode];
    if (instr.opcode == OP_NOP) {
        // don't print
    } else if (instr.opcode == OP_UNOP) {
        printf("    %d <- %s%s ", instr.result, get_unop_name((UnaryOp)instr.left), get_type_suffix(f, instr));
        print_value_operand(f, instr.right);
        putchar('\n');
    } else {
        if (OP_HAS_RESULT(instr.opcode)) {
            printf("    %d <- %s%s", instr.result, opinfo->name, get_type_suffix(f, instr));
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
void print_block_code(Func *f, Block block) {
    printf(":%d", block);
    print_params(f, block);
    putchar('\n');
    BlockInfo *block_info = &f->blocks[block];
    for (int i = 0; i < vector_size(block_info->instrs); ++i) {
        Instr instr = block_info->instrs[i];
        print_instr(f, block, instr);
    }
}
void print_code(Func *f) {
    for (int i = 1; i < f->blocks_size; ++i) {
        print_block_code(f, i);
    }
}
