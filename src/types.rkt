#lang racket
(provide (all-defined-out))

;; Immediates
(define immediate-shift 3)

;; Pointers
(define ptr-mask #b111)

;; Integers
(define int-shift   (+ immediate-shift 1))
(define type-int    #b0000)
(define mask-int    #b1111)

;; Chars
(define char-shift   (+ immediate-shift 2))
(define type-char    #b01000)
(define mask-char    #b11111)

;; Strings
(define type-string  #b011)

;; Bools
(define mask-bool   #b1111111)
(define val-true    #b0011000)
(define val-false   #b0111000)

;; IO
(define val-eof    #b1011000)
(define val-void   #b1111000)

;; Inductive Data
(define type-box  #b001)
(define type-cons #b010)
(define val-empty #b10011000)

;; Functions
(define type-proc #b100)

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

        [(= b val-eof)   eof]
        [(= b val-void)  (void)]
        [(= b val-empty) '()]

        [else (error "invalid bits")]))

(define (immediate->bits v)
  (cond [(eof-object? v) val-eof]
        [(integer? v)    (arithmetic-shift v int-shift)]

        ;; In this case, we can't simply shift, because the char flag
        ;; is comprised of 01, and not merely 0
        ;;
        ;; bitwise-ior is the bitwise inclusive or. The idea is simple:
        ;; we convert the char to int for bit manipulation, we shift it
        ;; to produce the zeroes and then:
        ;; (bitwise-ior #b01 #b00) -> #b01
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]

        [(eq? v #t) val-true]
        [(eq? v #f) val-false]

        [(void? v)  val-void]
        [(empty? v) val-empty]))
