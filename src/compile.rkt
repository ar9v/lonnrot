#lang racket
(provide compile)
(require "ast.rkt" "types.rkt" a86/ast)

;; compile: Expr -> Asm (x86 AST)
;; compile takes an expression represented in our
;; AST (the result from parsing) and emits an x86
;; AST representation
;;
;; This function wraps the actual source compilation
;; between the "boilerplate" that precedes the entry
;; label in the `.s` file
(define (compile expr)
  (prog (Label 'entry)
        (compile-e expr)
        (Ret)))

;; compile-e: AST -> Asm (x86 AST)
(define (compile-e expr)
  (match expr
    [(Int i)           (compile-value i)]
    [(Bool b)          (compile-value b)]
    [(Char c)          (compile-value c)]
    [(Prim1 op expr)   (compile-prim1 op expr)]
    [(If e1 e2 e3)     (compile-if e1 e2 e3)]))


;; Expressions

(define (compile-value v)
  (seq (Mov 'rax (value->bits v))))

(define (compile-prim1 op expr)
  (seq (compile-e expr)
       (match op
         ['add1 (seq (Add 'rax (value->bits 1)))]
         ['sub1 (seq (Sub 'rax (value->bits 1)))]
         ['zero?
          (let ([l1 (gensym)])
            (seq (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]
         ['char?
          (let ([l1 (gensym)])
            (seq (And 'rax mask-char)
                 (Xor 'rax type-char)
                 (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]

         ;; To convert between values, the idea is simple:
         ;; we strip their respective type tags, and then
         ;; add the target type's type tag
         ['char->integer
          (seq (Sar 'rax char-shift)
               (Sal 'rax int-shift))]
         ['integer->char
          (seq (Sar 'rax int-shift)
               (Sal 'rax char-shift)
               (Xor 'rax type-char))])))

(define (compile-if e1 e2 e3)
  (let ([l1 (gensym 'if)]
        [l2 (gensym 'if)])
    (seq (compile-e e1)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))
