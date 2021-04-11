#lang racket
(provide interp)
(require "ast.rkt")

;; interp: AST -> Int
(define (interp e)
  (match e
    [(Int i) i]
    [(Prim1 op e) (interp-prim1 op (interp e))]))

;; interp-prim1: Symbol x Integer -> Integer
(define (interp-prim1 op integer)
  (match op
    ['add1 (add1 integer)]
    ['sub1 (sub1 integer)]))
