#include "type.h"
#include "fnv.h"

#include <stdlib.h>


#define EXPAND_IMPLEMENTATION
#define NAME U64ToU64
#define KEY_TYPE uint64_t
#define VALUE_TYPE uint64_t
#define HASH_FUNC(x) hashutil_uint64_hash(x)
#include "hashtable.h"

#define EXPAND_IMPLEMENTATION
#define NAME U32Map
#define KEY_TYPE uint32_t
#define VALUE_TYPE uint32_t
#define HASH_FUNC(x) hashutil_uint32_hash(x)
#include "hashtable.h"

#define EXPAND_IMPLEMENTATION
#define NAME HashedU32ToI32
#define KEY_TYPE uint32_t
#define VALUE_TYPE int32_t
#define HASH_FUNC(x) (x)
#include "hashtable.h"


static TypeInfo *type_infos;
static uint32_t type_infos_size;
static uint32_t type_infos_capacity;

static Type alloc_type(void) {
    if (type_infos_size >= type_infos_capacity) {
        type_infos_capacity = type_infos_capacity ? type_infos_capacity * 2 : 128;
        type_infos = realloc(type_infos, sizeof(TypeInfo) * type_infos_capacity);
    }
    return type_infos_size++;
}

const TypeInfo *get_type_info(Type type) {
    assert(type > TYPE_NONE);
    assert(type < type_infos_size);
    return &type_infos[type];
}

static bool type_info_equal(const TypeInfo *a, const TypeInfo *b) {
    if (a->kind != b->kind) {
        return false;
    }
    if (a->size != b->size) {
        return false;
    }
    if (a->align != b->align) {
        return false;
    }
    if (a->target_type != b->target_type) {
        return false;
    }
    if (a->fields_size != b->fields_size) {
        return false;
    }
    if (a->fields) {
        for (uint32_t i = 0; i < a->fields_size; ++i) {
            const FieldInfo *fa = a->fields + i;
            const FieldInfo *fb = b->fields + i;
            if (strcmp(fa->name, fb->name)) {
                return false;
            }
            if (fa->type != fb->type) {
                return false;
            }
            if (fa->offset != fb->offset) {
                return false;
            }
        }
    }
    return true;
}

static uint32_t type_info_hash(const TypeInfo *t) {
    uint32_t hash = FNV_SEED;
    hash = fnv1a(&t->kind, sizeof(t->kind), hash);
    hash = fnv1a(&t->size, sizeof(t->size), hash);
    hash = fnv1a(&t->align, sizeof(t->align), hash);
    if (t->target_type) {
        const TypeInfo *target_info = get_type_info(t->target_type);
        hash = fnv1a(&target_info->hash, sizeof(target_info->hash), hash);
    }
    if (t->fields) {
        for (uint32_t i = 0; i < t->fields_size; ++i) {
            FieldInfo *f = t->fields + i;
            const TypeInfo *ft = get_type_info(f->type);
            hash = fnv1a(f->name, strlen(f->name), hash);
            hash = fnv1a(&ft->hash, sizeof(ft->hash), hash);
            hash = fnv1a(&f->offset, sizeof(f->offset), hash);
        }
    }
    return hash;
}

static void copy_type_info(TypeInfo *dst, const TypeInfo *src) {
    *dst = *src;
    if (src->fields) {
        dst->fields = malloc(sizeof(FieldInfo) * src->fields_size);
        memcpy(dst->fields, src->fields, sizeof(FieldInfo) * src->fields_size);
    }
}


static U32Map typemap;

static Type intern_type_into(TypeInfo *type_info, Type into_type) {
    for (uint32_t salt = 0; salt < 10000; ++salt) {
        uint32_t hash = type_info_hash(type_info);
        hash = fnv1a(&salt, sizeof(salt), hash);
        type_info->hash = hash;

        Type found;
        if (!U32Map_get(&typemap, type_info->hash, &found)) {
            if (into_type == TYPE_NONE) {
                into_type = alloc_type();
            }
            copy_type_info(&type_infos[into_type], type_info);
            U32Map_put(&typemap, type_info->hash, into_type);
            return into_type;
        }
        if (type_info_equal(type_info, get_type_info(found))) {
            assert(into_type == TYPE_NONE);
            return found;
        }
    }
    assert(0 && "should not happen");
    return TYPE_NONE;
}
static Type intern_type(TypeInfo *type_info) {
    return intern_type_into(type_info, TYPE_NONE);
}

Type get_ptr_type(Type target_type) {
    TypeInfo ptr_info = {
        .kind = TK_PTR,
        .size = sizeof(void *),
        .align = sizeof(void *),
        .target_type = target_type,
    };
    return intern_type(&ptr_info);
}
Type get_slice_type(Type item_type) {
    TypeInfo slice_info = {
        .kind = TK_SLICE,
        .size = sizeof(void *) * 2,
        .align = sizeof(void *),
        .target_type = item_type,
    };
    return intern_type(&slice_info);
}
Type get_array_type(uint32_t count, Type item_type) {
    const TypeInfo *item_info = get_type_info(item_type);
    TypeInfo array_info = {
        .kind = TK_ARRAY,
        .size = item_info->size * count,
        .align = item_info->align,
        .target_type = item_type,
    };
    return intern_type(&array_info);
}

static Type get_tuple_or_struct_type(TypeKind kind, uint32_t count, FieldInfo *fields) {
    assert(kind == TK_TUPLE || kind == TK_STRUCT);
    uint32_t offset = 0;
    uint32_t max_align = 0;
    for (uint32_t i = 0; i < count; ++i) {
        FieldInfo *field = fields + i;
        const TypeInfo *type_info = get_type_info(field->type);
        if (type_info->align > max_align) {
            max_align = type_info->align;
        }
        //if (offset % info->align)
        // TODO: field alignment
        field->offset = offset;
        offset += type_info->size;
    }
    // TODO: struct alignment
    TypeInfo type_info = {
        .kind = TK_TUPLE,
        .size = offset,
        .align = max_align,
        .fields = fields,
        .fields_size = count,
    };
    return intern_type(&type_info);
}

Type get_struct_type(uint32_t count, FieldInfo *fields) {
    return get_tuple_or_struct_type(TK_STRUCT, count, fields);
}

#define MAX_TUPLE_FIELDS 16
Type get_tuple_type(uint32_t count, const Type *types) {
    static const char *names[MAX_TUPLE_FIELDS] = {
        "0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15",
    };
    assert(count <= MAX_TUPLE_FIELDS);
    assert(count > 0);
    FieldInfo fields[MAX_TUPLE_FIELDS];
    for (uint32_t i = 0; i < count; ++i) {
        FieldInfo *field = fields + i;
        field->name = names[i];
        field->type = types[i];
        field->offset = 0;
    }
    return get_tuple_or_struct_type(TK_TUPLE, count, fields);
}

static void init_primitive_type(Type prim_type, TypeKind kind, uint32_t size) {
    assert(prim_type >= TYPE_BOOL && prim_type <= TYPE_F64);
    Type ptr_type = TYPE_PTR_BOOL + (prim_type - TYPE_BOOL);
    Type slice_type = TYPE_SLICE_BOOL + (prim_type - TYPE_BOOL);

    TypeInfo prim_info = {
        .kind = kind,
        .size = size,
        .align = size,
    };
    Type result = intern_type_into(&prim_info, prim_type);
    assert(result == prim_type);

    TypeInfo ptr_info = {
        .kind = TK_PTR,
        .size = sizeof(void *),
        .align = sizeof(void *),
        .target_type = prim_type,
    };
    result = intern_type_into(&ptr_info, ptr_type);
    assert(result == ptr_type);

    TypeInfo slice_info = {
        .kind = TK_SLICE,
        .size = sizeof(void *) * 2,
        .align = sizeof(void *),
        .target_type = prim_type,
    };
    result = intern_type_into(&slice_info, slice_type);
    assert(result == slice_type);
}

void init_types(void) {
    type_infos_size = TYPE_PREDEFINED_COUNT;
    type_infos_capacity = 128;
    type_infos = malloc(sizeof(TypeInfo) * type_infos_capacity);
    type_infos[TYPE_PAIR].kind = TK_PAIR;
    init_primitive_type(TYPE_BOOL, TK_BOOL, sizeof(bool));
    init_primitive_type(TYPE_I8, TK_INT, sizeof(int8_t));
    init_primitive_type(TYPE_I16, TK_INT, sizeof(int16_t));
    init_primitive_type(TYPE_I32, TK_INT, sizeof(int32_t));
    init_primitive_type(TYPE_I64, TK_INT, sizeof(int64_t));
    init_primitive_type(TYPE_U8, TK_UINT, sizeof(uint8_t));
    init_primitive_type(TYPE_U16, TK_UINT, sizeof(uint16_t));
    init_primitive_type(TYPE_U32, TK_UINT, sizeof(uint32_t));
    init_primitive_type(TYPE_U64, TK_UINT, sizeof(uint64_t));
    init_primitive_type(TYPE_F32, TK_REAL, sizeof(float));
    init_primitive_type(TYPE_F64, TK_REAL, sizeof(double));
    assert(type_infos_size == TYPE_PREDEFINED_COUNT);
}