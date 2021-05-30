#lang racket
(provide interp-prim0 interp-prim1 interp-prim2)

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
    ;; Arithmetic
    ['add1          (if (integer? v) (add1 v) 'err)]
    ['sub1          (if (integer? v) (sub1 v) 'err)]
    ['-             (if (integer? v) (- v)    'err)]

    ;; Predicates
    ['zero?         (if (integer? v) (zero? v) 'err)]
    ['null?         (null? v)]
    ['integer?      (integer? v)]
    ['boolean?      (boolean? v)]
    ['string?       (string? v)]
    ['char?         (char? v)]
    ['eof-object?   (eof-object? v)]

    ;; Booleans
    ['not?          (not v)]

    ;; Conversions
    ['integer->char (if (codepoint? v) (integer->char v) 'err)]
    ['char->integer (if (char? v) (char->integer v) 'err)]

    ;; I/O, Effects
    ['write-byte    (if (byte? v) (write-byte v) 'err)]
    ['displayln     (displayln v)]

    ;; Inductive Data
    ['string-length (string-length v)]
    ['box           (box v)]
    ['unbox         (if (box? v) (unbox v) 'err)]

    ['car           (if (pair? v) (car v) 'err)]
    ['cdr           (if (pair? v) (cdr v) 'err)]))


(define (interp-prim2 p v1 v2)
  (match p
    ;; Arithmetic
    ['+ (if (and (integer? v1) (integer? v2)) (+ v1 v2) 'err)]
    ['- (if (and (integer? v1) (integer? v2)) (- v1 v2) 'err)]

    ['< (if (and (integer? v1) (integer? v2)) (< v1 v2) 'err)]
    ['> (if (and (integer? v1) (integer? v2)) (> v1 v2) 'err)]
    ['= (if (and (integer? v1) (integer? v2)) (= v1 v2) 'err)]


    ;; Comparison
    ['eq? (eq? v1 v2)]

    ;; Inductive Data
    ['cons (cons v1 v2)]

    ['string-ref (string-ref v1 v2)]))

;; codepoint: Any -> Boolean
;; (codepoint v) checks if value `v` is within
;; Unicode code point ranges
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
