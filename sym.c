#include "sym.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    const char *name;
} SymbolInfo;

static int symbols_size;
static int symbols_capacity;
static SymbolInfo *symbols;
static SliceToU64 slice_to_index;

Symbol symbol_from_slice(Slice name) {
    uint64_t index = 0;
    if (SliceToU64_get(&slice_to_index, name, &index)) {
        return index;
    }

    char *name_str = malloc(name.len + 1);
    memcpy(name_str, name.ptr, name.len);
    name_str[name.len] = 0;
    name.ptr = name_str; // overwrite with the copy that we own
    
    if (symbols_size >= symbols_capacity) {
        symbols_capacity = symbols_capacity ? symbols_capacity * 2 : 128;
        symbols = realloc(symbols, sizeof(const char *) * symbols_capacity);

        if (symbols_size == 0) {
            Symbol none = symbol_from_str("");
            assert(none == SYMBOL_NONE);
            assert(symbols_size == 1);
        }
    }

    Symbol result = symbols_size++;
    symbols[result].name = name_str;
    SliceToU64_put(&slice_to_index, name, result);
    return result;
}

Symbol symbol_from_str(char *name) {
    return symbol_from_slice(slice_from_str(name));
}

const char *symbol_name(Symbol sym) {
    assert(sym > 0);
    assert(sym < symbols_size);
    return symbols[sym].name;
}
