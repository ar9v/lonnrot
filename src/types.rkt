#lang racket
(provide (all-defined-out))

(define int-shift 1)
(define type-int  #b0)
(define type-bool #b1)
(define val-true  #b01)
(define val-false #b11)

(define (bits->value b)
  (cond [(= type-int (bitwise-and b #b1))
         ;; We shift b right by 1 bit, i.e. we drop
         ;; the bit flag, so we have the integer representation
         (arithmetic-shift b (- int-shift))]
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(integer? v) (arithmetic-shift v int-shift)] ;; add the flag
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]))
