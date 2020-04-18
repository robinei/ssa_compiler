#pragma once

#include "ir.h"
#include "type.h"

typedef int32_t Block;
#define BLOCK_NONE 0

typedef union {
    bool b;
    int64_t i;
    uint64_t u;
    double f;
} StaticValue;

typedef struct {
    bool is_finished;
    Block idom_block; // immediate dominator. closest block which control flow always passes through to get here.
    Block redirect_block; // if this block does nothing but jump unconditionally to another block.
} BlockInfo;

typedef struct {
    Block block;
    Instr instr;
} VisibleInstr;

typedef struct {
    StaticValue *static_values;
    Instr *instrs;
    Type *types;
    int instrs_size;
    int instrs_capacity;
    int static_value_size;
    int static_value_capacity;

    BlockInfo *blocks;
    int blocks_size;
    int blocks_capacity;

    VisibleInstr *visible_instr_stack;
    int visible_instr_stack_size;
    int visible_instr_stack_capacity;

    Block entry_block;
    Block curr_block;

    // map used to keep track of visible instructions from dominating blocks, so we can reuse their results
    // instead of generating duplicate instructions.
    // the key is the instruction, and the value is the IRRef (instruction index).
    U64ToU64 visible_instr_map;

    HashedU32ToI32 instr_to_result;
} Func;

Func *func_new();

Block create_block(Func *f, Block idom_block);

IRRef get_static_bool(Func *f, bool v);
IRRef get_static_i8(Func *f, int8_t v);
IRRef get_static_i16(Func *f, int16_t v);
IRRef get_static_i32(Func *f, int32_t v);
IRRef get_static_i64(Func *f, int64_t v);
IRRef get_static_u8(Func *f, uint8_t v);
IRRef get_static_u16(Func *f, uint16_t v);
IRRef get_static_u32(Func *f, uint32_t v);
IRRef get_static_u64(Func *f, uint64_t v);
IRRef get_static_f32(Func *f, float v);
IRRef get_static_f64(Func *f, double v);

void emit_block(Func *f, Block block);
void emit_sloc(Func *f, int32_t row, int32_t col);
IRRef emit_pair(Func *f, IRRef left, IRRef right);
void emit_jump(Func *f, Block target);
void emit_jfalse(Func *f, IRRef cond, Block target);
IRRef emit_phi(Func *f, int count, const IRRef *refs);
IRRef emit_select(Func *f, IRRef cond, IRRef if_true, IRRef if_false);
void emit_ret(Func *f);
IRRef emit_unop(Func *f, UnaryOp unop, IRRef ref);
IRRef emit_binop(Func *f, OpCode binop, IRRef left, IRRef right);

void print_code(Func *f);
