#include <stdio.h>
#include <stdlib.h>
#include "Interpret.h"

Value eval(Exp p, Env *env) {
    Value v1, v2;
    switch (p->kind) {
    case is_EVar: {
        Value *result = env_lookup(env, p->u.evar_.ident_);
        if (result) {
            return *result;
        }
        printf("Couldn't read variable: %s\n", p->u.evar_.ident_);
        exit(EXIT_FAILURE);
    }
    case is_EInt:
        return make_int(p->u.eint_.integer_);
    case is_EMul:
        v1 = eval(p->u.emul_.exp_1, env);
        v2 = eval(p->u.emul_.exp_2, env);
        if (v1.kind == is_integer && v2.kind == is_integer) {
            return make_int(v1.value.num * v2.value.num);
        }
        printf("Cannot multiply functions, only integers\n");
        exit(EXIT_FAILURE);
    case is_EDiv:
        v1 = eval(p->u.ediv_.exp_1, env);
        v2 = eval(p->u.ediv_.exp_2, env);
        if (v1.kind == is_integer && v2.kind == is_integer) {
            return make_int(v1.value.num / v2.value.num);
        }
        printf("Cannot divide functions, only integers\n");
        exit(EXIT_FAILURE);
    case is_EAdd:
        v1 = eval(p->u.eadd_.exp_1, env);
        v2 = eval(p->u.eadd_.exp_2, env);
        if (v1.kind == is_integer && v2.kind == is_integer) {
            return make_int(v1.value.num + v2.value.num);
        }
        printf("Cannot add functions, only integers\n");
        exit(EXIT_FAILURE);
    case is_ESub:
        v1 = eval(p->u.esub_.exp_1, env);
        v2 = eval(p->u.esub_.exp_2, env);
        if (v1.kind == is_integer && v2.kind == is_integer) {
            return make_int(v1.value.num - v2.value.num);
        }
        printf("Cannot subtract functions, only integers\n");
        exit(EXIT_FAILURE);
    case is_ELt:
        v1 = eval(p->u.elt_.exp_1, env);
        v2 = eval(p->u.elt_.exp_2, env);
        if (v1.kind == is_integer && v2.kind == is_integer) {
            return make_int(v1.value.num < v2.value.num);
        }
        printf("Cannot compare functions, only integers\n");
        exit(EXIT_FAILURE);
    case is_EIf: {
        Value cond = eval(p->u.eif_.exp_1, env);
        if (cond.kind == is_integer) {
            return cond.value.num ? eval(p->u.eif_.exp_2, env) : eval(p->u.eif_.exp_3, env);
        }
        printf("Condition must be an integer (0 or 1)\n");
        exit(EXIT_FAILURE);
    }
    case is_EApp:
    case is_EAbs:
        printf("Not implemented\n");
        exit(EXIT_FAILURE);
    default:
        fprintf(stderr, "Error: bad kind field when evaluating Exp!\n");
        exit(1);
    }
}
