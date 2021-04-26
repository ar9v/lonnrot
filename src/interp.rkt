#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; interp: AST -> Value
;;
;; (interp e) takes an expression, e, that is represented as an AST,
;; and evaluates it. This is to say, it uses Racket's semantics.
(define (interp e)
  (match e
    ;; Values
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]

    ;; Primitives
    [(Prim0 p)
     (interp-prim0 p)]

    [(Prim1 op e)
     (interp-prim1 op (interp e))]

    ;; Control/Sequencing
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))
