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
    ['add1          (if (integer? v) (add1 v) 'err)]
    ['sub1          (if (integer? v) (sub1 v) 'err)]
    ['zero?         (if (integer? v) (zero? v) 'err)]
    ['char?         (char? v)]
    ['integer->char (if (codepoint? v) (integer->char v) 'err)]
    ['char->integer (if (char? v) (char->integer v) 'err)]
    ['eof-object?   (eof-object? v)]
    ['write-byte    (if (byte? v) (write-byte v) 'err)]))


;; codepoint: Any -> Boolean
;; (codepoint v) checks if value `v` is within
;; Unicode code point ranges
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
