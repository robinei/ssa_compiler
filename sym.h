#pragma once

#include "slice.h"

typedef uint32_t Symbol;
#define SYMBOL_NONE 0

Symbol symbol_from_slice(Slice name);
Symbol symbol_from_str(char *name);

const char *symbol_name(Symbol sym);
