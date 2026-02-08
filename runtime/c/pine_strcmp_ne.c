#include <string.h>
#include <stdbool.h>


bool pine_strcmp_ne(const char* a, const char* b) {
    return strcmp(a, b) != 0;
}
