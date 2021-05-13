#lang racket
(provide interp)
(require "ast.rkt"
         "interp-prim.rkt"
         "env.rkt")

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

;; REnv = ((Id Value) ...)

;; interp: AST -> Answer
;;
;; (interp e) takes an expression, e, that is represented as an AST,
;; and evaluates it. This is to say, it uses Racket's semantics.
;; It produces an Answer, which is either a Value or 'err and uses
;; an auxiliary function, `interp-env` which passes around the environment

(define (interp e)
  (interp-env e '()))


(define (interp-env e env)
  (match e
    ;; Values
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x) (lookup x env)]

    ;; Primitives
    [(Prim0 p)
     (interp-prim0 p)]

    [(Prim1 p e)
     (match (interp-env e env)
       ['err 'err]
       [v (interp-prim1 p v)])]

    [(Prim2 p e1 e2)
     (match (interp-env e1 env)
       ['err 'err]
       [v1 (match (interp-env e2 env)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]

    ;; Control
    [(If e1 e2 e3)
     (match (interp-env e1 env)
       ['err 'err]
       [v (if v
              (interp-env e2 env)
              (interp-env e3 env))])]

    ;; Sequencing
    [(Begin e1 e2)
     (match (interp-env e1 env)
       ['err 'err]
       [_ (interp-env e2 env)])]

    ;; Binding
    [(Let x e1 e2)
     (match (interp-env e1 env)
       ['err 'err]
       [v (interp-env e2 (ext env x v))])]))
