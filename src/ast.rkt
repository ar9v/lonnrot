#lang racket

(provide Int Bool Char Prim1 If)

;; data Expr =
;;  | Int Integer
;;  | Prim Op Expr
;;  | If Expr Expr Expr
;; data Op = 'add1 | 'sub1 | 'zero?
(struct Int (i)       #:prefab)
(struct Bool (b)      #:prefab)
(struct Char (c)      #:prefab)
(struct Prim1 (p e)   #:prefab)
(struct If (e1 e2 e3) #:prefab)
