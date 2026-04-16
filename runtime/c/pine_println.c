#include <stdio.h>

int pine_println(const char* s) {
    return printf("%s\n", s);
}

int pine_println_int(int d) {
    return printf("%d\n", d);
}

int pine_println_ld(long long ld) {
    return printf("%llu\n", ld);
}

int pine_println_float(float f) {
    return printf("%f\n", f);
}
