#lang racket
;; (provide mparse)
(require megaparsack)

;; This Racket file includes a version
;; of the hand-made parser for Lonnrot
;; (i.e. one that does not rely on the
;; Racket reader)


;; mparse: String -> Sexp
;;
;; mparse takes a `source` string, which is the
;; String representation of a Lonnrot program, and
;; produces its s-exp representation
(define (mparse source)
  source)
