#ifndef ENV_H
#define ENV_H

#include "Value.h"

typedef struct EnvEntry {
    char* name;
    Value value;
    struct EnvEntry* next;
} EnvEntry;

typedef struct Env {
    EnvEntry* entries[64];
    struct Env* outer;
} Env;

unsigned int hash(char* str);
Env* create_env(Env* outer);
Value* env_lookup(Env* env, char* name);
void env_define(Env* env, char* name, Value value);

#endif
