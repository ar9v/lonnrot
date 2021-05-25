#lang racket
(provide interp)
(require "ast.rkt"
         "interp-prim.rkt"
         "env.rkt"
         srfi/1)

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

;; REnv = [(Id Value) ...]
;; Defs = [Def]

;; interp: AST -> Answer
;;
;; (interp e) takes an expression, e, that is represented as an AST,
;; and evaluates it. This is to say, it uses Racket's semantics.
;; It produces an Answer, which is either a Value or 'err and uses
;; an auxiliary function, `interp-env` which passes around the environment

;; interp: Program -> Answer
;; Program: Function definitions and an expression
(define (interp p)
  (interp-env (desugar p) '()))

;; interp-env: Expr x Env x Defs -> Answer
;; interp-env: takes an AST Node, an Environment and
;; a list of function definitions and produces an answer with them
;; UPDATE (loot): since we have lambdas and desugaring, we don't need
;; to pass around the defs anymore
(define (interp-env e env)
  (match e
    ;; Values
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x) (lookup x env)]
    [(Lambda _ params body)
     (lambda vs
       ;; First, we check that the lambda has correct arity
       (if (= (length vs) (length params))
           ;; If it does, we simply produce the lambda by zipping
           ;; together the interpreter environment with the bindings
           (interp-env body (append (zip params vs) env))
           ;; If it doesn't we signal an error
           'err))]

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
    [(Begin args) (interp-begin args env)]

    ;; Binding
    [(Let x e1 e2)
     (match (interp-env e1 env)
       ['err 'err]
       [v (interp-env e2 (ext env x v))])]

    ;; This letrec implementation is, to my understanding,
    ;; closer to `fix` than regular old letrec. Check Dybvig's paper
    [(LetRec bindings body)
     (letrec ((env* (lambda ()
                      (append
                       (zip (map car bindings)
                            ;; η-expansion to delay evaluating r*
                            ;; relies on RHSs being functions
                            (map (lambda (l) (lambda vs (apply (interp-env l (env*)) vs)))
                                 (map cadr bindings)))
                       env))))
       (interp-env body (env*)))]

    ;; Function Application
    ;; Lonnrot is strict call by value, left to right evaluation of the arguments
    ;; and it is eval/apply (i.e. we evaluate the arguments and /then/ we check
    ;; if the function exists)
    [(App expr es)
     (let ([f (interp-env expr env)]
           [args (interp-env* es env)])

       (if (procedure? f)
           (apply f args)
           'err))]

    [(Prog '() e) (interp-env e env)]))


(define (interp-begin args env)
  (match args
    [(cons arg '()) (interp-env arg env)]
    [(cons a as)
     (match (interp-env a env)
       ['err 'err]
       [v (interp-begin as env)])]))

;; interp-env*: [Exprs] x Environment x Function Definitions
;; -> [Answers] | 'err
;;
;; interp-env*: Takes a list of expressions and returns either the
;; list of their answers or an error
(define (interp-env* es env)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e env)
       ['err 'err]
       [v (cons v (interp-env* es env))])]))

;; lookup-fn: Defn x [Defn] -> Defn | 'err
;; (lookup-fn fn defs) looks up fn in defs. It uses `findf` because it returns #f or
;; the first element that fulfills the predicate given as its first argument, instead of
;; the `cdr` of defs where the `car` fulfills the predicate.
;;
;; match-lambda is syntactic sugar for `(lambda (id) (match...))`
(define (lookup-fn fn defs)
  (findf (match-lambda [(Defn d _ _) (eq? fn d)])
         defs))
