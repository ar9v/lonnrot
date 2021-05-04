#lang racket
(provide compile)
(require "ast.rkt" "types.rkt" a86/ast)

;; Registers
;; rdi is one of the caller-saved registers, and
;; it will have to be used for, e.g., write-byte
(define rax 'rax)
(define rbx 'rbx)
(define rsp 'rsp)
(define rdi 'rdi)


;; compile: Expr -> Asm (x86 AST)
;; compile takes an expression represented in our
;; AST (the result from parsing) and emits an x86
;; AST representation
;;
;; This function wraps the actual source compilation
;; between the "boilerplate" that precedes the entry
;; label in the `.s` file
(define (compile expr)
  (prog
   ;; These are the C defined functions in the runtime
   (Extern 'peek_byte)
   (Extern 'read_byte)
   (Extern 'write_byte)
   (Extern 'raise_error)

   ;; Before calling functions from other object files (e.g.
   ;; the IO functions in io.c), the stack has to be
   ;; aligned to 16 bytes (according to Sys V ABI convention)
   ;; So we decrement a word from the stack pointer and then
   ;; restore it
   (Label 'entry)
   (Sub rsp 8)
   (compile-e expr)
   (Add rsp 8)
   (Ret)

   ;; Now, we handle errors
   (Label 'err)
   (Call 'raise_error)))

;; compile-e: AST -> Asm (x86 AST)
(define (compile-e expr)
  (match expr
    [(Int i)           (compile-value i)]
    [(Bool b)          (compile-value b)]
    [(Char c)          (compile-value c)]
    [(Eof)             (compile-value eof)]
    [(Prim0 p)         (compile-prim0 p)]
    [(Prim1 p expr)    (compile-prim1 p expr)]
    [(If e1 e2 e3)     (compile-if e1 e2 e3)]
    [(Begin e1 e2)     (compile-begin e1 e2)]))


;; Expressions
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

(define (compile-prim0 p)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (Call 'read_byte))]
    ['peek-byte (seq (Call 'peek_byte))]))

;; Since we have to check for errors, we'll have
;; some helper functions per primitive

;; Now, compile-prim1 requires logic for
;; compiling the primitive and the expression. The idea is that
;; with errors we can't indiscriminately generate code for
;; each primitive.
(define (compile-prim1 p expr)
  (seq (compile-e expr)
       (compile-p p)))


(define (compile-p p)
  (match p
    ;; Increment/Decrement
    ['add1
     (seq assert-integer
          (Add rax (value->bits 1)))]
    ['sub1
     (seq assert-integer
          (Sub rax (value->bits 1)))]

    ;; Predicates
    ['zero?
     (let ([l1 (gensym)])
       (seq assert-integer
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['char?
     (let ([l1 (gensym)])
       ;; AND gives us the last two bits
       ;; XOR will produce 0 if the last two bits are
       ;; the same, i.e. if the last bits are 01, type char
       (seq (And rax mask-char)
            (Xor rax type-char)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['eof-object?
     (let ([l1 (gensym)])
       (seq (Cmp rax val-eof)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]

    ;; Value conversions
    ;; To convert between values, the idea is simple:
    ;; we strip their respective type tags, and then
    ;; add the target type's type tag
    ['char->integer
     (seq assert-char
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq assert-integer
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]

    ;; IO
    ['write-byte
     (seq assert-byte
          (Mov rdi rax)
          (Call 'write_byte)
          (Mov rax val-void))]))


;; Conditional
(define (compile-if e1 e2 e3)
  (let ([l1 (gensym 'if)]
        [l2 (gensym 'if)])
    (seq (compile-e e1)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

;; Sequence
(define (compile-begin e1 e2)
  (seq (compile-e e1)
       (compile-e e2)))


;; Assertions
(define (assert-type mask type)
  (seq (Mov rbx rax)
       (And rbx mask)
       (Cmp rbx type)
       (Jne 'err)))

(define assert-integer
  (assert-type mask-int type-int))

(define assert-char
  (assert-type mask-char type-char))

;; This is basically the comparison
;; we made in the interpreter
(define assert-codepoint
  (let ([ok (gensym)])
    (seq assert-integer
         (Cmp rax (value->bits 0))
         (Jl 'err)
         (Cmp rax (value->bits 1114111))
         (Jg 'err)
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp 'err)
         (Label ok))))

(define assert-byte
  (seq assert-integer
       (Cmp rax (value->bits 0))
       (Jl 'err)
       (Cmp rax (value->bits 255))
       (Jg 'err)))
