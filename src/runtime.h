#ifndef RUNTIME_H
#define RUNTIME_H
int64_t entry();
extern FILE* in;
extern FILE* out;

#define HEAP_SIZE 10000
extern int64_t *heap;
#endif /* RUNTIME_H */
