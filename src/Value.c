#include "Value.h"

Value make_int(Integer n) {
    return (Value) {.kind = is_integer, .value.num = n};
}
