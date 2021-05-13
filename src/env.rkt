#lang racket
(provide lookup ext)

;; lookup: Id x Env -> Answer
;; (lookup x env) Looks for the /value/ associated to the
;; id `x` in the environment `env`
(define (lookup var env)
  (match env
    ['() 'err]
    [(cons (list id val) env)
     (if (symbol=? id var)
         val
         (lookup var env))]))

;; ext: Env x Id x Value -> Env
;; (ext env x v) extends environment `env` with the pair
;; (x . v)
(define (ext env x v)
  (cons (list x v) env))
