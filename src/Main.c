#include <stdio.h>
#include <stdlib.h>
#include "Interpret.h"
#include "../Abs/Parser.h"
#include "../Abs/Printer.h"

int main(int argc, char **argv) {
    FILE *input;
    Program parse_tree;
    char *file = NULL;

    if (argc > 1) {
        file = argv[1];
    }
    
    if (file) {
        input = fopen(file, "r");
        if (!input) {
            printf("Failed to open file!\n");
            exit(1);
        }
    } else {
        printf("No filename provided!\n");
        exit(1);
    }

    parse_tree = pProgram(input);
    fclose(input);
    
    if (parse_tree) {
        Env *env = create_env(NULL);
        Value v = eval(parse_tree->u.prog_.main_->u.dmain_.exp_, env);
        if (v.kind == is_integer) {
            printf("%d\n", v.value.num);
        }
        else {
            printf("main returned a function which is not allowed!");
            exit(EXIT_FAILURE);
        }
        free_Program(parse_tree);
    }

    return 0;
}
