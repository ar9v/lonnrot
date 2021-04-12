#lang racket
(provide interp-prim1)

;; interp-prim1: Symbol x Integer -> Integer
(define (interp-prim1 op integer)
  (match op
    ['add1 (add1 integer)]
    ['sub1 (sub1 integer)]))
