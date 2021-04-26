#lang racket
(provide parse)
(require "ast.rkt")

;; parse: Sexp -> Maybe Error
;;
;; (parse s) takes a symbolic expression, s, which
;; represents a Lonnrot Scheme program, and turns it
;; into AST form (a form which is defined in `ast.rkt`)
;;
;; The symbolic expression s represents the program parsed
;; by the Racket reader.
(define (parse s)
  ;; We either have a value or a primitive application
  ;; We use cond here because we want to evaluate the predicates
  ;; as opposed to checking for a particular structure (i.e. `match`)
  (cond [(integer? s) (Int s)]
        [(boolean? s) (Bool s)]
        [(char? s) (Char s)]
        [else
         (match s
           ;; Nullary primitives (Prim0)
           ['eof                    (Eof)]
           [(list 'read-byte)       (Prim0 'read-byte)]
           [(list 'peek-byte)       (Prim0 'peek-byte)]

           ;; Arithmetic primitives
           [(list 'add1 e)          (Prim1 'add1          (parse e))]
           [(list 'sub1 e)          (Prim1 'sub1          (parse e))]

           ;; Unary Primitives (Prim1)
           [(list 'zero? e)         (Prim1 'zero?         (parse e))]
           [(list 'char? e)         (Prim1 'char?         (parse e))]
           [(list 'write-byte e)    (Prim1 'write-byte    (parse e))]
           [(list 'eof-object? e)   (Prim1 'eof-object?   (parse e))]
           [(list 'integer->char e) (Prim1 'integer->char (parse e))]
           [(list 'char->integer e) (Prim1 'char->integer (parse e))]

           ;; Control/Sequencing operators
           [(list 'begin e1 e2)
            (Begin (parse e1) (parse e2))]
           [(list 'if e1 e2 e3)
            (If (parse e1) (parse e2) (parse e3))]

           ;; Error
           [_ (error "Parsing error")])]))
