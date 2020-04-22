#include "ir.h"

const OpCodeInfo opcode_info[OPCODE_COUNT] = {
    [OP_NOP]    = { "nop",      OPERAND_NONE,   OPERAND_NONE },
    [OP_BLOCK]  = { "block",    OPERAND_NONE,   OPERAND_BLOCK },
    [OP_SLOC]   = { "sloc",     OPERAND_ROW,    OPERAND_COL },
    [OP_JUMP]   = { "jump",     OPERAND_NONE,   OPERAND_BLOCK },
    [OP_JFALSE] = { "jfalse",   OPERAND_REF,    OPERAND_BLOCK },
    [OP_RET]    = { "ret",      OPERAND_REF,    OPERAND_NONE },
    [OP_COPY]   = { "copy",     OPERAND_REF,    OPERAND_REF },
    
    [OP_PHI]    = { "phi",      OPERAND_REF,    OPERAND_REF },
    [OP_SELECT] = { "select",   OPERAND_REF,    OPERAND_REF },
    [OP_PAIR]   = { "pair",     OPERAND_REF,    OPERAND_REF },
    [OP_CALL]   = { "call",     OPERAND_FUNC,   OPERAND_REF },
    [OP_ARG]    = { "arg",      OPERAND_ARGPOS, OPERAND_NONE },

    [OP_UNOP]   = { "unop",     OPERAND_UNOP,   OPERAND_REF },
    
    [OP_AND]    = { "and",      OPERAND_REF,    OPERAND_REF },
    [OP_OR]     = { "or",       OPERAND_REF,    OPERAND_REF },
    [OP_EQ]     = { "eq",       OPERAND_REF,    OPERAND_REF },
    [OP_NEQ]    = { "neq",      OPERAND_REF,    OPERAND_REF },
    [OP_LT]     = { "lt",       OPERAND_REF,    OPERAND_REF },
    [OP_GT]     = { "gt",       OPERAND_REF,    OPERAND_REF },
    [OP_LTEQ]   = { "lteq",     OPERAND_REF,    OPERAND_REF },
    [OP_GTEQ]   = { "gteq",     OPERAND_REF,    OPERAND_REF },

    [OP_ADD]    = { "add",      OPERAND_REF,    OPERAND_REF },
    [OP_SUB]    = { "sub",      OPERAND_REF,    OPERAND_REF },
    [OP_MUL]    = { "mul",      OPERAND_REF,    OPERAND_REF },
    [OP_DIV]    = { "div",      OPERAND_REF,    OPERAND_REF },
    [OP_MIN]    = { "min",      OPERAND_REF,    OPERAND_REF },
    [OP_MAX]    = { "max",      OPERAND_REF,    OPERAND_REF },
    
    [OP_POW]    = { "pow",      OPERAND_REF,    OPERAND_REF },

    [OP_MOD]    = { "mod",      OPERAND_REF,     OPERAND_REF },
    
    [OP_BAND]   = { "band",     OPERAND_REF,    OPERAND_REF },
    [OP_BOR]    = { "bor",      OPERAND_REF,    OPERAND_REF },
    [OP_BXOR]   = { "bxor",     OPERAND_REF,    OPERAND_REF },
    [OP_BSHL]   = { "bshl",     OPERAND_REF,    OPERAND_REF },
    [OP_BSHR]   = { "bshr",     OPERAND_REF,    OPERAND_REF },
};


bool is_result_reusable(OpCode opcode) {
    if (!OP_HAS_RESULT(opcode) || opcode == OP_PHI || opcode == OP_CALL) {
        return false;
    }
    const OpCodeInfo *opinfo = &opcode_info[opcode];
    return (opinfo->left == OPERAND_REF && opinfo->right == OPERAND_REF)
        || (opinfo->left == OPERAND_NONE && opinfo->right == OPERAND_REF)
        || (opinfo->left == OPERAND_REF && opinfo->right == OPERAND_NONE);
}
