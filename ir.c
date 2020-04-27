#include "ir.h"

const OpCodeInfo opcode_info[OPCODE_COUNT] = {
    [OP_NOP]    = { "nop",      OPERAND_NONE,   OPERAND_NONE },
    [OP_JUMP]   = { "jump",     OPERAND_NONE,   OPERAND_BLOCK },
    [OP_JFALSE] = { "jfalse",   OPERAND_VALUE,  OPERAND_BLOCK },
    [OP_RET]    = { "ret",      OPERAND_VALUE,  OPERAND_NONE },
    [OP_COPY]   = { "copy",     OPERAND_VALUE,  OPERAND_VALUE },
    
    [OP_SELECT] = { "select",   OPERAND_VALUE,  OPERAND_VALUE },
    [OP_PAIR]   = { "pair",     OPERAND_VALUE,  OPERAND_VALUE },
    [OP_CALL]   = { "call",     OPERAND_FUNC,   OPERAND_VALUE },

    [OP_UNOP]   = { "unop",     OPERAND_UNOP,   OPERAND_VALUE },
    
    [OP_AND]    = { "and",      OPERAND_VALUE,  OPERAND_VALUE },
    [OP_OR]     = { "or",       OPERAND_VALUE,  OPERAND_VALUE },
    [OP_EQ]     = { "eq",       OPERAND_VALUE,  OPERAND_VALUE },
    [OP_NEQ]    = { "neq",      OPERAND_VALUE,  OPERAND_VALUE },
    [OP_LT]     = { "lt",       OPERAND_VALUE,  OPERAND_VALUE },
    [OP_GT]     = { "gt",       OPERAND_VALUE,  OPERAND_VALUE },
    [OP_LTEQ]   = { "lteq",     OPERAND_VALUE,  OPERAND_VALUE },
    [OP_GTEQ]   = { "gteq",     OPERAND_VALUE,  OPERAND_VALUE },

    [OP_ADD]    = { "add",      OPERAND_VALUE,  OPERAND_VALUE },
    [OP_SUB]    = { "sub",      OPERAND_VALUE,  OPERAND_VALUE },
    [OP_MUL]    = { "mul",      OPERAND_VALUE,  OPERAND_VALUE },
    [OP_DIV]    = { "div",      OPERAND_VALUE,  OPERAND_VALUE },
    [OP_MIN]    = { "min",      OPERAND_VALUE,  OPERAND_VALUE },
    [OP_MAX]    = { "max",      OPERAND_VALUE,  OPERAND_VALUE },
    
    [OP_POW]    = { "pow",      OPERAND_VALUE,  OPERAND_VALUE },

    [OP_MOD]    = { "mod",      OPERAND_VALUE,  OPERAND_VALUE },
    
    [OP_BAND]   = { "band",     OPERAND_VALUE,  OPERAND_VALUE },
    [OP_BOR]    = { "bor",      OPERAND_VALUE,  OPERAND_VALUE },
    [OP_BXOR]   = { "bxor",     OPERAND_VALUE,  OPERAND_VALUE },
    [OP_BSHL]   = { "bshl",     OPERAND_VALUE,  OPERAND_VALUE },
    [OP_BSHR]   = { "bshr",     OPERAND_VALUE,  OPERAND_VALUE },
};


bool is_result_reusable(OpCode opcode) {
    if (!OP_HAS_RESULT(opcode) || opcode == OP_CALL) {
        return false;
    }
    const OpCodeInfo *opinfo = &opcode_info[opcode];
    return (opinfo->left == OPERAND_VALUE && opinfo->right == OPERAND_VALUE)
        || (opinfo->left == OPERAND_NONE && opinfo->right == OPERAND_VALUE)
        || (opinfo->left == OPERAND_VALUE && opinfo->right == OPERAND_NONE);
}
