#lang racket
(provide (all-defined-out))

(define int-shift       1)
(define char-shift      2)
(define type-int      #b0)
(define type-char    #b01)
(define mask-char    #b11)
(define val-true    #b011)
(define val-false   #b111)
(define val-eof    #b1011)
(define val-void   #b1111)

(define (bits->value b)
  (cond [(= type-int (bitwise-and b #b1))
         ;; We shift b right by 1 bit, i.e. we drop
         ;; the bit flag, so we have the integer representation
         (arithmetic-shift b (- int-shift))]
        [(= type-char (bitwise-and b mask-char))
         ;; Likewise, but we now shift by two bits
         (arithmetic-shift b (- char-shift))]
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(integer? v) (arithmetic-shift v int-shift)] ;; add the flag
        [(char? v)
         ;; In this case, we can't simply shift, because the char flag
         ;; is comprised of 01, and not merely 0
         ;;
         ;; bitwise-ior is the bitwise inclusive or. The idea is simple:
         ;; we convert the char to int for bit manipulation, we shift it
         ;; twice to produce the two zeroes and then:
         ;; (bitwise-ior #b01 #b00) -> #b01
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]))
