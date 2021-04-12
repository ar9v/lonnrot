#lang racket
(provide interp)
(require "ast.rkt" "interp-prim1.rkt")

;; interp: AST -> Int
(define (interp e)
  (match e
    [(Int i) i]
    [(Prim1 op e) (interp-prim1 op (interp e))]
    [(IfZero e1 e2 e3)
     (if (zero? (interp e1))
         (interp e2)
         (interp e3))]))
