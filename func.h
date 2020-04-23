#pragma once

#include "ir.h"

typedef int32_t Block;
#define BLOCK_NONE 0

typedef int32_t BlockArg;

typedef struct {
    IRRef ref;
    BlockArg next;
} BlockArgEntry;

typedef union {
    Type t;
    bool b;
    int64_t i;
    uint64_t u;
    double f;
} StaticValue;

typedef struct {
    Block idom_block : 30; // immediate dominator. closest block which control flow always passes through to get here.
    bool is_filled : 1;
    bool is_sealed : 1;

    int source_count;
    Block sources[2]; // sources are chained from a block using the corresponding next_source slot in each chained block.
    Block next_source[2];
    Block targets[2]; // jump target is targets[0], and jfalse target (if any) is targets[1].
    IRRef jumps[2];
    BlockArg args[2]; // arguments to the corresponding jump instructions
    
    IRRef entry;
    IRRef params; // chained OP_PARAM instructions. name is left (Symbol), next is right (IRRef).
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

    BlockArgEntry *args;
    int args_size;
    int args_capacity;

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

    U64ToU64 env_key_to_ref;
} Func;

Func *func_new();

Block create_block(Func *f, Block idom_block);
void seal_block(Func *f, Block block);

IRRef get_static_type(Func *f, Type type);
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
IRRef emit_select(Func *f, IRRef cond, IRRef if_true, IRRef if_false);
IRRef emit_param(Func *f, Symbol name, Type type);
void emit_ret(Func *f, IRRef ref);
IRRef emit_unop(Func *f, UnaryOp unop, IRRef ref);
IRRef emit_binop(Func *f, OpCode binop, IRRef left, IRRef right);

IRRef add_param(Func *f, Block block, Symbol sym, Type type);
void define_symbol(Func *f, Block block, Symbol sym, IRRef ref);
IRRef lookup_symbol(Func *f, Block block, Symbol sym);

void print_code(Func *f);
