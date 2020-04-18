#pragma once

#include <assert.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#define MAX(x, y) (((x) > (y)) ? (x) : (y))

typedef uint32_t Type;
enum {
    TYPE_NONE,

    TYPE_PAIR, // only used for compile-time IR pairs, to chain instruction operands.
    
    TYPE_BOOL,
    TYPE_I8,
    TYPE_I16,
    TYPE_I32,
    TYPE_I64,
    TYPE_U8,
    TYPE_U16,
    TYPE_U32,
    TYPE_U64,
    TYPE_F32,
    TYPE_F64,

    TYPE_PTR_BOOL,
    TYPE_PTR_I8,
    TYPE_PTR_I16,
    TYPE_PTR_I32,
    TYPE_PTR_I64,
    TYPE_PTR_U8,
    TYPE_PTR_U16,
    TYPE_PTR_U32,
    TYPE_PTR_U64,
    TYPE_PTR_F32,
    TYPE_PTR_F64,
    
    TYPE_SLICE_BOOL,
    TYPE_SLICE_I8,
    TYPE_SLICE_I16,
    TYPE_SLICE_I32,
    TYPE_SLICE_I64,
    TYPE_SLICE_U8,
    TYPE_SLICE_U16,
    TYPE_SLICE_U32,
    TYPE_SLICE_U64,
    TYPE_SLICE_F32,
    TYPE_SLICE_F64,

    TYPE_PREDEFINED_COUNT,
};

#define TYPE_IS_NUM(t) ((t) >= TYPE_I8 && (t) <= TYPE_F64)
#define TYPE_IS_INT(t) ((t) >= TYPE_I8 && (t) <= TYPE_U64)
#define TYPE_IS_SINT(t) ((t) >= TYPE_I8 && (t) <= TYPE_I64)
#define TYPE_IS_UINT(t) ((t) >= TYPE_U8 && (t) <= TYPE_U64)
#define TYPE_IS_REAL(t) ((t) >= TYPE_F32 && (t) <= TYPE_F64)

typedef enum {
    TK_NONE,

    TK_PAIR,
    
    TK_BOOL,
    TK_INT,
    TK_UINT,
    TK_REAL,

    TK_PTR,
    TK_SLICE,
    TK_ARRAY,
    
    // tuple and struct types are the same,
    // except tuple fields are numbered, and struct fields are named.
    TK_TUPLE,
    TK_STRUCT,
} TypeKind;

typedef struct {
    const char *name; // will be "0", "1" etc. for tuples
    Type type;
    uint32_t offset;
} FieldInfo;

typedef struct {
    TypeKind kind;
    uint32_t size;
    uint32_t align;
    
    // for ptr/slice/array
    Type target_type;
    
    // for tuple/struct
    FieldInfo *fields;
    uint32_t fields_size;

    uint32_t hash; // hash of this type (and a salt, changed until we get a unique hash). this field is not itself hashed!
} TypeInfo;

void init_types(void);

const TypeInfo *get_type_info(Type type);

Type get_ptr_type(Type target_type);
Type get_slice_type(Type item_type);
Type get_array_type(uint32_t count, Type item_type);

Type get_struct_type(uint32_t count, FieldInfo *fields); // field offsets will be rewritten.
Type get_tuple_type(uint32_t count, const Type *types);



#define EXPAND_INTERFACE
#define NAME U32Map
#define KEY_TYPE uint32_t
#define VALUE_TYPE uint32_t
#include "hashtable.h"

#define EXPAND_INTERFACE
#define NAME U64ToU64
#define KEY_TYPE uint64_t
#define VALUE_TYPE uint64_t
#include "hashtable.h"

#define EXPAND_INTERFACE
#define NAME HashedU32ToI32
#define KEY_TYPE uint32_t
#define VALUE_TYPE int32_t
#include "hashtable.h"