#lang racket
(provide interp interp-env-heap)
(require "ast.rkt"
         "env.rkt"
         "interp-prims-heap.rkt")


;; In our normal interpreter (and overall semantics)
;; we had that:
;;
;; Answer = Value | err
;;
;; Now, in the explicit semantics, we have
;;
;; Answer* = (cons Heap Value*)
;; Value* =
;; | Integer
;; | Boolean
;; | Char
;; | Eof
;; | Empty
;; | Var
;; | Void
;; | '()
;; | (list 'box Address)
;; | (list 'cons Address)
;;
;; And:
;; Heap = [Value*]
;; REnv = [(Id Value*)]

;; interp: AST Expr -> Answer
(define (interp e)
  (unload (interp-env-heap e '() '())))


;; interp-env-heap: AST Expr x REnv x Heap -> Answer*
(define (interp-env-heap e env heap)
  (match e
    [(Int i)    (cons heap i)]
    [(Bool b)   (cons heap b)]
    [(Char c)   (cons heap c)]
    [(Eof)      (cons heap eof)]
    [(Empty)    (cons heap '())]
    [(Var x)    (cons heap (lookup x env))]

    [(Prim0 'void)      (cons h (void))]
    [(Prim0 'peek-byte) (cons h (peek-byte))]
    [(Prim0 'read-byte) (cons h (read-byte))]

    [(Prim1 p e)
     (match (interp-env-heap e env heap)
       ['err 'err]
       [(cons h v)
        (interp-prim1 p v h)])]

    [(Prim2 p e1 e2)
     (match (interp-env-heap e1 env heap)
       ['err 'err]
       [(cons h v1)
        (match (interp-env-heap e2 env h)
          ['err 'err]
          [(cons h v2)
           (interp-prim2 p v1 v2 h)])])]

    [(If e1 e2 e2)
     (match (interp-env-heap e1 env heap)
       ['err 'err]
       [(cons h v)
        (if v
            (interp-env-heap e1 env heap)
            (interp-env-heap e2 env heap))])]

    [(Begin e1 e2)
     (match (interp-env-heap e1 env heap)
       ['err 'err]
       [(cons h _)
        (interp-env-heap e2 env heap)])]

    [(Let x e1 e2)
     (match (interp-env-heap e1 env heap)
       ['err 'err]
       [(cons h v)
        (interp-env-heap e2 (ext env x v) h)])]))
