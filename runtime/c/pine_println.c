#include <stdio.h>

int pine_println(const char* s) {
    return printf("%s\n", s);
}

int pine_println_int(int d) {
    return printf("%d\n", d);
}
