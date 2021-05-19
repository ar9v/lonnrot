#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "types.h"
#include "runtime.h"

FILE* in;
FILE* out;
void (*error_handler)();
int64_t *heap;

void print_result(int64_t);
void print_char(int64_t);
void print_cons(int64_t);

void error_exit() {
    printf("err\n");
    exit(1);
}

void raise_error() {
    return error_handler();
}

int main(int argc, char **argv) {
    // We initialize
    // - stdin/out
    // - the error function used when 'err is produced
    // - the heap
    in = stdin;
    out = stdout;
    error_handler = &error_exit;
    heap = malloc(8 * HEAP_SIZE);

    // The heap is passed to rdi. We know the argument to the
    // main entry function is stored in rdi due to Sys V ABI
    // convention
    int64_t result = entry(heap);

    // Since we know have boxes and pairs, before we print
    // the result, we might need to print a pair's quote

    if(CONS_TYPE_TAG == (PTR_TYPE_MASK & result)) printf("'");
    print_result(result);

    if(result != VAL_VOID) printf("\n");

    free(heap);
    return 0;
}

void print_cons(int64_t address) {
    // We store the car and the cdr "backwards" so that we
    // don't have to juggle values in registers during code
    // generation (since cons is binary, and the second argument
    // in binary primitives is in rsp and so on)
    int64_t car = *((int64_t *)((address + 8) ^ CONS_TYPE_TAG));
    int64_t cdr = *((int64_t *)((address + 0) ^ CONS_TYPE_TAG));

    print_result(car);

    if(cdr == VAL_EMPTY) {
        // Leave it as it is
    }
    // The cdr may be another cons cell; if it is
    // we must call the procedure recursively
    else if(CONS_TYPE_TAG == (PTR_TYPE_MASK & cdr)) {
        printf(" ");
        print_cons(cdr);
    }
    // But if it's not, we simply use the dot notation
    // for strict pairs (i.e. it is not a list)
    else {
        printf(" . ");
        print_result(cdr);
    }
}

void print_result(int64_t result) {
    if(CONS_TYPE_TAG == (PTR_TYPE_MASK & result)) {
        printf("(");
        print_cons(result);
        printf(")");
    }
    else if(BOX_TYPE_TAG == (PTR_TYPE_MASK & result)) {
        // To print a box, we:
        // - Prefix it with the box identifier
        // - XOR the result with the tag, i.e. we set the last three
        //   bits to all zeroes and ask to print the value in the resulting
        //   address.
        printf("#&");
        print_result(*((int64_t *)(result ^ BOX_TYPE_TAG)));
    }
    else if(PROC_TYPE_TAG == (PTR_TYPE_MASK & result)) {
       printf("<procedure>");
    }
    else if(INT_TYPE_TAG == (INT_TYPE_MASK & result)) {
        printf("%" PRId64, result >> INT_SHIFT);
    }
    else if (CHAR_TYPE_TAG == (CHAR_TYPE_MASK & result)) {
        print_char(result);
    }
    else {
        switch(result) {
            case VAL_TRUE:
                printf("#t\n");
                break;
            case VAL_FALSE:
                printf("#f\n");
                break;
            case VAL_EOF:
                printf("#<eof>\n");
                break;
            case VAL_EMPTY:
                printf("()");
                break;
            case VAL_VOID:
                break;
        }
    }
}
