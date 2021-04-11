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
    [(Prim1 op expr) (compile-prim1 op expr)]
    [(Int i)         (compile-integer i)]))

;; compile-prim1: Symbol x
(define (compile-prim1 op expr)
  (seq (compile-e expr)
       (match op
         ['add1 (seq (Add 'rax 1))]
         ['sub1 (seq (Sub 'rax 1))])))

(define (compile-integer i)
  (seq (Mov 'rax i)))
