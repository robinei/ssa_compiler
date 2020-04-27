#pragma once

#define VECTOR_INITIAL_CAPACITY 16

#define vector_free(V) (free((int *)(V) - 2))

#define _vector__size(V) (((int *)(V))[-1])
#define _vector__capacity(V) (((int *)(V))[-2])

#define vector_size(V) ((V) ? _vector__size(V) : 0)
#define vector_capacity(V) ((V) ? _vector__capacity(V) : 0)

#define vector_empty(V) (!(V) || !_vector__size(V))
#define vector_last(V) ((V)[_vector__size(V) - 1])

#define vector_pop(V) ((V)[--_vector__size(V)])
#define vector_push(V, X) \
    do { \
        if ((V) && _vector__size(V) < _vector__capacity(V)) { \
            (V)[_vector__size(V)++] = X; \
        } else { \
            if (V) { \
                _vector__capacity(V) *= 2; \
                (V) = (void *)((int *)realloc((int *)(V) - 2, sizeof(int)*2 + sizeof(*(V))*_vector__capacity(V)) + 2); \
            } else { \
                (V) = (void *)((int *)malloc(sizeof(int)*2 + sizeof(*(V))*VECTOR_INITIAL_CAPACITY) + 2); \
                _vector__capacity(V) = VECTOR_INITIAL_CAPACITY; \
                _vector__size(V) = 0; \
            } \
            (V)[_vector__size(V)++] = X; \
        } \
    } while (0)

#define vector_erase(V, I) \
    do { \
        if (V) { \
            memcpy((V) + (I), (V) + (I) + 1, (--_vector__size(V) - (I)) * sizeof(*(V))); \
        } \
    } while (0)

#define vector_foreach(V, X) \
    for (int __index = 0, __max_index = vector_size(V); \
         __index < __max_index && ((X) = (V)[__index], 1); \
         ++__index)
