#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; Before, the semantics dictated that expressions such
;; as (zero? #f) or (add1 #f) had undefined behavior. Now
;; we say that evaluating a program produces an `answer` which
;; is
;;
;; Answer = Value | 'err
;; Value =
;; | Integer
;; | Boolean
;; | Char
;; | Eof
;; | Void

;; interp: AST -> Answer
;;
;; (interp e) takes an expression, e, that is represented as an AST,
;; and evaluates it. This is to say, it uses Racket's semantics.
;; It produces an Answer, which is either a Value or 'err
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

    [(Prim1 p e)
     (match (interp e)
       ['err 'err]
       [v (interp-prim1 p v)])]

    ;; Control
    [(If e1 e2 e3)
     (match (interp e1)
       ['err 'err]
       [v (if v (interp e2) (interp e3))])]

    ;; Sequencing
    [(Begin e1 e2)
     (match (interp e1)
       ['err 'err]
       [_ (interp e2)])]))
