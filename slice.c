#include "slice.h"
#include "murmur3.h"
#include "fnv.h"
#include <stdlib.h>
#include <string.h>

Slice slice_from_str_len(char *str, uint32_t len) {
    Slice result;
    result.ptr = str;
    result.len = result.cap = len;
    return result;
}

Slice slice_from_str(char *str) {
    Slice result;
    result.ptr = str;
    result.len = result.cap = strlen(str);
    return result;
}

bool slice_equals(Slice a, Slice b) {
    if (a.len != b.len) {
        return false;
    }
    return !memcmp(a.ptr, b.ptr, a.len);
}

int slice_cmp(Slice a, Slice b) {
    uint32_t min_len = a.len <= b.len ? a.len : b.len;
    uint32_t res = memcmp(a.ptr, b.ptr, min_len);
    return res != 0 ? res : a.len - b.len;
}

int slice_str_cmp(Slice a, char *b_str) {
    Slice b;
    b.ptr = b_str;
    b.len = b.cap = strlen(b_str);
    return slice_cmp(a, b);
}

/*Slice slice_dup(Slice s, Allocator *a) {
    Slice result;
    result.len = result.cap = s.len;
    result.ptr = allocate(a, s.len + 1);
    memcpy(result.ptr, s.ptr, s.len);
    result.ptr[s.len] = '\0';
    return result;
}*/

Slice slice_span(Slice a, Slice b) {
    Slice c;
    c.ptr = a.ptr <= b.ptr ?
        a.ptr :
        b.ptr;
    c.len = c.cap = a.ptr + a.len >= b.ptr + b.len ?
        (a.ptr + a.len) - c.ptr :
        (b.ptr + b.len) - c.ptr;
    return c;
}

uint32_t slice_hash_murmur(Slice s) {
    uint32_t hash;
    MurmurHash3_x86_32(s.ptr, s.len, 0, &hash);
    return hash;
}

uint32_t slice_hash_fnv1a(Slice s) {
    return fnv1a((unsigned char *)s.ptr, s.len, FNV_SEED);
}


#define EXPAND_IMPLEMENTATION
#define NAME        SliceToU64
#define KEY_TYPE    Slice
#define VALUE_TYPE  uint64_t
#define HASH_FUNC   slice_hash_murmur
#define EQUAL_FUNC  slice_equals
#include "hashtable.h"
