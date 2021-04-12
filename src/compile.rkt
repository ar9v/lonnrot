#lang racket
(provide compile)
(require "ast.rkt" a86/ast)

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
    [(Int i)           (compile-integer i)]
    [(Prim1 op expr)   (compile-prim1 op expr)]
    [(IfZero e1 e2 e3) (compile-ifzero e1 e2 e3)]))


;; Expressions

(define (compile-integer i)
  (seq (Mov 'rax i)))

(define (compile-prim1 op expr)
  (seq (compile-e expr)
       (match op
         ['add1 (seq (Add 'rax 1))]
         ['sub1 (seq (Sub 'rax 1))])))

(define (compile-ifzero e1 e2 e3)
  (let ([l1 (gensym 'if)]
        [l2 (gensym 'if)])
    (seq (compile-e e1)
         (Cmp 'rax 0)
         (Je l1)
         (compile-e e3)
         (Jmp l2)
         (Label l1)
         (compile-e e2)
         (Label l2))))
