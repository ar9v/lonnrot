#lang racket

(provide Int Prim1)

;; data Expr =
;;  | Int Integer
;;  | Prim Op Expr
;; data Op = 'add1 | 'sub1
(struct Int (i)     #:prefab)
(struct Prim1 (p e) #:prefab)
