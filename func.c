#include "func.h"
#include "fnv.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

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

    f->types[IRREF_FALSE] = TYPE_BOOL;
    f->types[IRREF_TRUE] = TYPE_BOOL;
    f->static_values[IRREF_TRUE].b = true;
    f->static_value_size = 3; // NONE/FALSE/TRUE
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
    return block;
}

static IRRef get_static_value(Func *f, Type type, StaticValue static_value) {
    assert(type != TYPE_NONE);
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
    assert(left <= INSTR_LEFT_MAX);
    assert(right <= INSTR_RIGHT_MAX);
    assert(!f->blocks[f->curr_block].is_finished);

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

    f->curr_block = block;
    emit_instr(f, OP_BLOCK, 0, block, TYPE_NONE);
}

void emit_sloc(Func *f, int32_t row, int32_t col) {
    emit_instr(f, OP_SLOC, row, col, TYPE_NONE);
}

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

void emit_jump(Func *f, Block target) {
    BlockInfo *curr_block_info = &f->blocks[f->curr_block];
    if (curr_block_info->is_finished) {
        // if JUMP follows a JFALSE that specialized into a JUMP, then we'll already be finished
        return;
    }
    if (f->instrs[f->instrs_size - 1].opcode == OP_BLOCK) {
        // only the BLOCK instruction was emitted before this unconditional jump, so this block is unnecessary
        curr_block_info->redirect_block = target;
    }
    emit_instr(f, OP_JUMP, 0, target, TYPE_NONE);
    curr_block_info->is_finished = true;
}
void emit_jfalse(Func *f, IRRef cond, Block target) {
    assert(f->types[cond] == TYPE_BOOL);
    if (IRREF_IS_STATIC(cond)) {
        if (f->static_values[cond].b) {
            // no jump, so skip
        } else {
            emit_jump(f, target); // unconditional jump, since condition is false
        }
    } else {
        emit_instr(f, OP_JFALSE, cond, target, TYPE_NONE);
    }
}

IRRef emit_phi(Func *f, int count, const IRRef *refs) {
    assert(count >= 2);
    IRRef left = refs[0];
    Type type = f->types[left];
    IRRef right;
    if (count == 2) {
        right = refs[1];
        assert(type == f->types[right]);
    } else {
        for (int i = 1; i < count; ++i) {
            assert(type == f->types[refs[i]]);
        }
        right = emit_create_ref_list(f, count - 1, refs + 1);
    }
    return emit_instr(f, OP_PHI, left, right, type);
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

void emit_ret(Func *f) {
    BlockInfo *curr_block_info = &f->blocks[f->curr_block];
    if (curr_block_info->is_finished) {
        // if RET follows a JFALSE that specialized into a JUMP, then we'll already be finished
        return;
    }
    emit_instr(f, OP_RET, 0, 0, TYPE_NONE);
    curr_block_info->is_finished = true;
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
static Type get_unop_result_type(UnaryOp unop, Type right) {
    if (unop == UNOP_NEG) { assert(TYPE_IS_NUM(right)); return right; }
    if (unop == UNOP_NOT) { assert(right == TYPE_BOOL); return right; }
    if (unop == UNOP_BNOT && TYPE_IS_INT(right)) { return right; }
    assert(false && "illegal type for unary operator");
}
IRRef emit_unop(Func *f, UnaryOp unop, IRRef ref) {
    Type result_type = get_unop_result_type(unop, f->types[ref]);
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
    assert(false && "illegal types for binary operator");
}
IRRef emit_binop(Func *f, OpCode binop, IRRef left, IRRef right) {
    Type left_type = f->types[left];
    Type right_type = f->types[right];
    Type result_type = get_binop_result_type(binop, left_type, right_type);

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






static void print_static_value(Func *f, IRRef ref) {
    assert(IRREF_IS_STATIC(ref));
    StaticValue *static_value = &f->static_values[ref];
    const TypeInfo *type_info = get_type_info(f->types[ref]);
    switch (type_info->kind) {
    case TK_BOOL:
        printf(" <%s>", static_value->b ? "true" : "false");
        break;
    case TK_INT:
        printf(" <%ld>", static_value->i);
        break;
    case TK_UINT:
        printf(" <%lu>", static_value->u);
        break;
    case TK_REAL:
        printf(" <%f>", static_value->f);
        break;
    default:
        printf(" %d", ref);
        break;
    }
}
static void print_value_operand(Func *f, IRRef ref) {
    if (IRREF_IS_STATIC(ref)) {
        print_static_value(f, ref);
    } else {
        printf(" %d", ref);
    }
}
static void print_operand(Func *f, OperandType operand_type, int32_t operand) {
    switch (operand_type) {
    case OPERAND_NONE:
        break;
    case OPERAND_REF:
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
void print_code(Func *f) {
    for (int i = 0; i < f->instrs_size; ++i) {
        Instr instr = f->instrs[i];
        const OpCodeInfo *opinfo = &opcode_info[instr.opcode];
        if (instr.opcode == OP_NOP) {
            printf("    nop\n");
        } else if (instr.opcode == OP_BLOCK) {
            printf(":%d\n", instr.right);
        } else if (instr.opcode == OP_UNOP) {
            printf("    %d <- %s", i, get_unop_name((UnaryOp)instr.left));
            printf("%s", get_type_suffix(f, instr));
            print_value_operand(f, instr.right);
        } else {
            if (OP_HAS_RESULT(instr.opcode)) {
                printf("    %d <- %s", i, opinfo->name);
            } else {
                printf("    %s", opinfo->name);
            }
            printf("%s", get_type_suffix(f, instr));
            print_operand(f, opinfo->left, instr.left);
            print_operand(f, opinfo->right, instr.right);
            putchar('\n');
        }
    }
}
