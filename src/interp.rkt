#lang racket
(provide interp)
(require "ast.rkt" "interp-prim1.rkt")

;; interp: AST -> Int
(define (interp e)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Prim1 op e) (interp-prim1 op (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))
