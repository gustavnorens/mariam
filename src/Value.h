#ifndef VALUE_H
#define VALUE_H

#include "../Abs/Absyn.h"

typedef struct {
    Exp body;
    struct Env *env;
    char *name;
} Closure;

typedef struct {
    enum { is_integer, is_closure } kind;
    union {
        Integer num;
        Closure fun;
    } value;
} Value;

Value make_int(Integer n);

#endif
