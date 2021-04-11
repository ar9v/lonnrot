#lang racket
(provide check-compiler)
(require rackunit
         a86/interp
         "../src/interp.rkt"
         "../src/compile.rkt"
         "../src/ast.rkt")


(define (check-compiler expr)
  (check-eqv? (interp expr)
              (asm-interp (compile expr))))
