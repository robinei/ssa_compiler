#pragma once

#include <stdint.h>
#include <stdbool.h>

typedef struct {
    char *ptr;
    uint32_t len;
    uint32_t cap;
} Slice;

#define SLICE_EMPTY ((Slice) { "", 0, 0, })

Slice slice_from_str_len(char *str, uint32_t len);
Slice slice_from_str(char *str);
bool slice_equals(Slice a, Slice b);
int slice_cmp(Slice a, Slice b);
int slice_str_cmp(Slice a, char *b_str);
//Slice slice_dup(Slice s, Allocator *a); /* result is 0-terminated */

/* requires that both slices point into the same buffer */
Slice slice_span(Slice a, Slice b);

uint32_t slice_hash_murmur(Slice s);
uint32_t slice_hash_fnv1a(Slice s);


#define EXPAND_INTERFACE
#define NAME        SliceToU64
#define KEY_TYPE    Slice
#define VALUE_TYPE  uint64_t
#include "hashtable.h"
