#include <stdio.h>
#include <inttypes.h>
#include "types.h"
#include "runtime.h"

// These functions, which deal with IO,
// are called by code that is produced
// by the compiler

// This is essentially the way that we can print
// without having to do all the bit-twiddling
// involved in printing at the assembly level
int64_t read_byte() {
    char c = getc(in);

    return (c == EOF) ?
        VAL_EOF :
        (int64_t) (c << INT_SHIFT);
}

int64_t peek_byte() {
    char c = getc(in);
    ungetc(c, in);

    return (c == EOF) ?
        VAL_EOF :
        (int64_t) (c << INT_SHIFT);
}

int64_t write_byte(int64_t c) {
    int64_t codepoint = c >> INT_SHIFT;
    putc((char) codepoint, out);

    return 0;
}
