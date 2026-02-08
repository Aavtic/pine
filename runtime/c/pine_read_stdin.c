#include <stdio.h>
#include <stdlib.h>

const char* pine_read_line_stdin(char *prompt) {
    char* input = (char *)malloc(1024 * sizeof(char));
    printf("%s", prompt);
    scanf("%s", input);
    return input;
}
