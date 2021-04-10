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

;; compile-e: Expr -> Asm (x86 AST)
(define (compile-e expr)
  (match expr
    [(Int i) (seq (Mov 'rax i))]))
