#include <stdio.h>
#include <inttypes.h>
#include "types.h"

void print_codepoint(int64_t);

// print_char takes a value v and
// 1. removes the char tag (hence the shift)
// 2. prints the hash character which is used in Scheme
// 3. either prints a special character or the actual value
void print_char(int64_t v) {
    int64_t codepoint = v >> CHAR_SHIFT;

    printf("#\\");

    switch(codepoint) {
        case 0:
            printf("nul");
            break;
        case 8:
            printf("backspace");
            break;
        case 9:
            printf("tab");
            break;
        case 10:
            printf("tab");
            break;
        case 11:
            printf("vtab");
            break;
        case 12:
            printf("page");
            break;
        case 13:
            printf("return");
            break;
        case 32:
            printf("space");
            break;
        case 127:
            printf("rubout");
            break;
        default:
            print_codepoint(v);
    }
}

// print_codepoint prints value v according to
// the UTF-8 standard
void print_codepoint(int64_t v) {
    int64_t codepoint = v >> CHAR_SHIFT;

    // Anything below 128 is the same as ASCII
    // (i.e. traditional ASCII only requires 7 bits)...
    if(codepoint < 128) {
        printf("%c", (char) codepoint);
    }
    // ...this accounts for the second block (U+07FF)...
    else if(codepoint < 2048) {
        // In this set of characters, we use two bytes, hence the
        // double printing. (Byte1 Byte2)
        //
        // Its encoding structure is:
        // Byte 1    | Byte 2
        // 110x xxxx | 10xx xxxx
        //
        // Therefore, printing a codepoint means
        // 1. Remove the 6 rightmost bits
        // 2. OR the resulting bits with 192, which has the
        //    binary representation 1100 0000, and thus yields
        //    Byte 1 of the UTF8 structure
        // 3. Mask the rightmost 6 bits, since 63 is represented
        //    in binary with 6 1's. This yields the 6 rightmost bits
        // 4. OR the result of 3 with 128, which yields Byte 2
        printf("%c%c",
               (char) (codepoint >> 6) | 192,
               ((char) codepoint & 63) | 128);
    }
    // ...and this for U+FFFF, for which we apply the same logic
    else if(codepoint < 65536) {
        printf("%c%c%c",
               (char) (codepoint >> 12) | 224,
               ((char) (codepoint >> 6) & 63) | 128,
               ((char) codepoint & 63) | 128);
    }
    // ...U+10000 and upwards
    else {
        printf("%c%c%c%c",
               (char) (codepoint >> 18) | 240,
               ((char)(codepoint >> 12) & 63) | 128,
               ((char)(codepoint >> 6) & 63) | 128,
               ((char) codepoint & 63) | 128);
    }
}
