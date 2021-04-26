#lang racket
(provide interp-prim0 interp-prim1)

;; interp-prim0: Op0 -> Value
;;
;; (interp-prim0 p) takes a symbol representing
;; a primitive p and evaluates the corresponding
;; primitive in Racket
(define (interp-prim0 p)
  (match p
    ['read-byte (read-byte)]
    ['peek-byte (peek-byte)]
    ['void      (void)]))

;; interp-prim1: Op1 x Value -> Value
;;
;; (interp-prim1 p) takes a symbol representing
;; primitive p and a value v and produces a value
;; as a result of evaluating the operation according
;; to Racket's equivalent primitive application
(define (interp-prim1 op v)
  (match op
    ['add1          (add1 v)]
    ['sub1          (sub1 v)]
    ['zero?         (zero? v)]
    ['char?         (char? v)]
    ['integer->char (integer->char v)]
    ['char->integer (char->integer v)]
    ['eof-object?   (eof-object? v)]
    ['write-byte    (write-byte v)]))
