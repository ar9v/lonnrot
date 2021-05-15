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
  (match p
    [(Prog defs e)
     (interp-env e '() defs)]))

;; interp-env: Expr x Env x Defs -> Answer
;; interp-env: takes an AST Node, an Environment and
;; a list of function definitions and produces an answer with them
(define (interp-env e env defs)
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
     (match (interp-env e env defs)
       ['err 'err]
       [v (interp-prim1 p v)])]

    [(Prim2 p e1 e2)
     (match (interp-env e1 env defs)
       ['err 'err]
       [v1 (match (interp-env e2 env defs)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]

    ;; Control
    [(If e1 e2 e3)
     (match (interp-env e1 env defs)
       ['err 'err]
       [v (if v
              (interp-env e2 env defs)
              (interp-env e3 env defs))])]

    ;; Sequencing
    [(Begin e1 e2)
     (match (interp-env e1 env defs)
       ['err 'err]
       [_ (interp-env e2 env defs)])]

    ;; Binding
    [(Let x e1 e2)
     (match (interp-env e1 env defs)
       ['err 'err]
       [v (interp-env e2 (ext env x v) defs)])]

    ;; Function Application
    ;; Lonnrot is strict call by value, left to right evaluation of the arguments
    ;; and it is eval/apply (i.e. we evaluate the arguments and /then/ we check
    ;; if the function exists)
    [(App f es)
     (match (interp-env* es env defs)
       ;; If interp-env* succeeds, we get a list of answers...
       [(list vs ...)
        ;; ...and we need to check if the function exists
        (match (lookup-fn f defs)
          ;; If the function exists, then check its arity
          [(Defn fn params body)
           (if (= (length params) (length vs))
               (interp-env body (zip params vs) defs)
               'err)])]

       ;; If we get anything other than a list of answers we have an error
       [_ 'err])]))

;; interp-env*: [Exprs] x Environment x Function Definitions
;; -> [Answers] | 'err
;;
;; interp-env*: Takes a list of expressions and returns either the
;; list of their answers or an error
(define (interp-env* es env defs)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e env defs)
       ['err 'err]
       [v (cons v (interp-env* es env defs))])]))

;; lookup-fn: Defn x [Defn] -> Defn | 'err
;; (lookup-fn fn defs) looks up fn in defs. It uses `findf` because it returns #f or
;; the first element that fulfills the predicate given as its first argument, instead of
;; the `cdr` of defs where the `car` fulfills the predicate.
;;
;; match-lambda is syntactic sugar for `(lambda (id) (match...))`
(define (lookup-fn fn defs)
  (findf (match-lambda [(Defn d _ _) (eq? fn d)])
         defs))
