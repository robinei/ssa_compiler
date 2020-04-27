#pragma once

#include "ir.h"

typedef struct Func Func;
typedef int32_t Block;
typedef int32_t Value;

#define VALUE_UNIT -1
#define VALUE_FALSE -2
#define VALUE_TRUE -3
#define VALUE_IS_STATIC(value) ((value) < 0)

Func *func_new();

void set_current_block(Func *f, Block block);
Block get_current_block(Func *f);

Block create_block(Func *f);
Value add_block_param(Func *f, Block block, Type type);
void seal_block(Func *f, Block block);

Value get_static_type(Func *f, Type type);
Value get_static_bool(Func *f, bool v);
Value get_static_i8(Func *f, int8_t v);
Value get_static_i16(Func *f, int16_t v);
Value get_static_i32(Func *f, int32_t v);
Value get_static_i64(Func *f, int64_t v);
Value get_static_u8(Func *f, uint8_t v);
Value get_static_u16(Func *f, uint16_t v);
Value get_static_u32(Func *f, uint32_t v);
Value get_static_u64(Func *f, uint64_t v);
Value get_static_f32(Func *f, float v);
Value get_static_f64(Func *f, double v);

Value emit_pair(Func *f, Value left, Value right);
Value emit_select(Func *f, Value cond, Value if_true, Value if_false);
void emit_ret(Func *f, Value value);
Value emit_unop(Func *f, UnaryOp unop, Value value);
Value emit_binop(Func *f, OpCode binop, Value left, Value right);

void emit_jump(Func *f, Block target);
void emit_jfalse(Func *f, Value cond, Block target);
void emit_jump_with_args(Func *f, Block target, int arg_count, Value *args);
void emit_jfalse_with_args(Func *f, Value cond, Block target, int arg_count, Value *args);

void define_symbol(Func *f, Block block, Symbol sym, Value value);
Value lookup_symbol(Func *f, Block block, Symbol sym);

void postprocess_code(Func *f);
void print_code(Func *f);
