#pragma once

#include "type.h"

typedef enum OpCode {
    OP_NOP,     // does nothing. useful for removing instructions without moving any. stripped before the end.
    OP_JUMP,    // right is target block id. jumps unconditionally.
    OP_JFALSE,  // left is condition, right is target block id. jumps if condition is false.
    OP_RET,     // left is return value. return from current function.
    OP_COPY,    // left register is copied to right. has no result. not used in SSA form.
    
    // all instructions not having a result must come before SELECT!
    
    OP_SELECT,  // left is bool condition; right is PAIR to choose from (true -> pair.left, false -> pair.right).

    // create a pair of the arguments. used to chain arguments to CALL instruction in a linked list.
    OP_PAIR,
    
    OP_CALL,    // left is function id. right is a single argument or an argument list: pair(a, pair(b, c)) etc.

    // unary operators take an UnaryOp operator in left, and a single operand in right, and output the same type.
    OP_UNOP,

    // binary operators require that left and right have equal types, and outputs same type (except comparisons).

    OP_AND,     // bool -> bool -> bool
    OP_OR,      // bool -> bool -> bool

    OP_EQ,      // any -> any -> bool
    OP_NEQ,     // any -> any -> bool
    OP_GT,      // any -> any -> bool
    OP_LT,      // any -> any -> bool
    OP_GTEQ,    // any -> any -> bool
    OP_LTEQ,    // any -> any -> bool

    OP_ADD,     // num -> num -> num
    OP_SUB,     // num -> num -> num
    OP_MUL,     // num -> num -> num
    OP_DIV,     // num -> num -> num
    OP_MIN,     // num -> num -> num
    OP_MAX,     // num -> num -> num
    
    OP_POW,     // real -> real -> real

    OP_MOD,     // int -> int -> int

    OP_BAND,    // int -> int -> int
    OP_BOR,     // int -> int -> int
    OP_BXOR,    // int -> int -> int
    OP_BSHL,    // int -> int -> int
    OP_BSHR,    // int -> int -> int

    // binary operators must come last!

    OPCODE_COUNT
} OpCode;

typedef enum UnaryOp {
    UNOP_NEG,   // num  -> num
    UNOP_NOT,   // bool -> bool
    UNOP_BNOT,  // int  -> int
} UnaryOp;

#define OP_HAS_RESULT(opcode) ((opcode) >= OP_SELECT)
#define OP_IS_BINOP(opcode) ((opcode) >= OP_AND)
#define OP_IS_NUM_BINOP(opcode) ((opcode) >= OP_ADD)
#define OP_IS_BOOL_BINOP(opcode) ((opcode) >= OP_AND && (opcode) <= OP_OR)
#define OP_IS_CMP_BINOP(opcode) ((opcode) >= OP_EQ && (opcode) <= OP_LTEQ)
#define OP_IS_RELCMP_BINOP(opcode) ((opcode) >= OP_GT && (opcode) <= OP_LTEQ)
#define OP_IS_EQCMP_BINOP(opcode) ((opcode) >= OP_EQ && (opcode) <= OP_NEQ)
#define OP_IS_REAL_BINOP(opcode) ((opcode) >= OP_POW && (opcode) <= OP_POW)
#define OP_IS_INT_BINOP(opcode) ((opcode) >= OP_MOD && (opcode) <= OP_BSHR)

#define INSTR_OPERAND_BITS  19
#define INSTR_OPCODE_BITS   (64 - INSTR_OPERAND_BITS * 3)

#define INSTR_OPCODE_MAX    ((1 << INSTR_OPCODE_BITS) - 1)
#define INSTR_OPERAND_MAX   ((1 << INSTR_OPERAND_BITS) - 1)

static_assert(OPCODE_COUNT <= INSTR_OPCODE_MAX, "not enough bits for all opcodes");

typedef union Instr {
    struct {
        uint64_t opcode : INSTR_OPCODE_BITS;
        int64_t  left   : INSTR_OPERAND_BITS;
        int64_t  right  : INSTR_OPERAND_BITS;
        int64_t  result : INSTR_OPERAND_BITS;
    };
    uint64_t u64_repr;
} Instr;


typedef enum OperandType {
    OPERAND_NONE,
    OPERAND_VALUE,
    OPERAND_BLOCK,
    OPERAND_UNOP,
    OPERAND_FUNC,
} OperandType;

typedef struct OpCodeInfo {
    const char *name;
    OperandType left;
    OperandType right;
} OpCodeInfo;

extern const OpCodeInfo opcode_info[OPCODE_COUNT];

bool is_result_reusable(OpCode opcode);

