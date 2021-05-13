#lang racket

(provide
 Eof Empty Int Bool Char Var
 Prim0 Prim1 Prim2
 If Begin
 Let)

;; ast.rkt
;;
;; These are structs that represent nodes in the
;; Lonnrot Scheme AST. For practical purposes, think
;; of these as types, in the Haskell/OCaml sense of
;; being defined by `data` or `type`
;;
;; Notably, these mirror the structure of the a86
;; tree, but are not necessarily the same (!!)

;; data Expr =
;;    Eof
;;  | Empty ;; nil, or '()
;;  | Int Integer
;;  | Bool Boolean
;;  | Char Character
;;  | Var Id
;;  | Prim0 Op0
;;  | Prim1 Op1 Expr
;;  | Prim2 Op2 Expr Expr
;;  | If Expr Expr Expr
;;  | Begin Expr Expr
;;  | Let Id Expr Expr
;;
;; data Op0 = 'read-byte  | 'peek-byte  | 'void
;;
;; data Op1 =
;;    'add1
;;  | 'sub1
;;  | 'zero?
;;  | 'char?
;;  | 'integer->char
;;  | 'char->integer
;;  | 'write-byte
;;  | 'eof-object?
;;  | 'box | 'unbox
;;  | 'car | 'cdr
;;
;; data Op2 = '+ | '- | 'cons
;;
;; Id = Symbol

;; Values
(struct Eof ()          #:prefab)
(struct Empty ()        #:prefab)
(struct Int (i)         #:prefab)
(struct Bool (b)        #:prefab)
(struct Char (c)        #:prefab)
(struct Var (x)         #:prefab)

;; Primitives
(struct Prim0 (p)       #:prefab)
(struct Prim1 (p e)     #:prefab)
(struct Prim2 (p e1 e2) #:prefab)

;; Conditional, Sequencing
(struct If (e1 e2 e3)   #:prefab)
(struct Begin (e1 e2)   #:prefab)

;; Variable binding
(struct Let (x e1 e2)   #:prefab)
