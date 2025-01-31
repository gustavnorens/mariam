#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Env.h"

unsigned int hash(char* str) {
    unsigned int hash = 5381;
    int c;
    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c;
    }
    return hash % 64;
}

Env* create_env(Env* outer) {
    Env* env = malloc(sizeof(Env));
    memset(env->entries, 0, sizeof(env->entries));
    env->outer = outer;
    return env;
}

Value* env_lookup(Env* env, char* name) {
    while (env) {
        unsigned int h = hash(name);
        EnvEntry* entry = env->entries[h];

        while (entry) {
            if (strcmp(entry->name, name) == 0) {
                return &entry->value;
            }
            entry = entry->next;
        }
        env = env->outer;
    }
    return NULL;
}

void env_define(Env* env, char* name, Value value) {
    unsigned int h = hash(name);
    
    EnvEntry* entry = malloc(sizeof(EnvEntry));
    entry->name = (char *) strdup(name);
    entry->value = value;

    entry->next = env->entries[h];
    env->entries[h] = entry;
}
