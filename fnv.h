#pragma once

#include <stdint.h>

#define FNV_PRIME 0x01000193
#define FNV_SEED 0x811C9DC5

static uint32_t fnv1a(const void *ptr, uint32_t len, uint32_t hash) {
    const uint8_t *p = ptr;
    while (len--) {
        hash = (*p++ ^ hash) * FNV_PRIME;
    }
    return hash;
}
