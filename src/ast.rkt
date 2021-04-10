#lang racket

(provide Int)

;; data Expr = Int Integer
(struct Int (i) #:prefab)
