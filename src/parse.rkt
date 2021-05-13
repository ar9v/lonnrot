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
  (cond [(integer? s)   (Int s)]
        [(boolean? s)   (Bool s)]
        [(char? s)      (Char s)]
        [(symbol? s)    (Var s)]
        [(empty? s)     (Empty)]
        [else
         (match s
           ;; Nullary primitives (Prim0)
           ['eof                    (Eof)]
           [(list 'read-byte)       (Prim0 'read-byte)]
           [(list 'peek-byte)       (Prim0 'peek-byte)]

           ;; Unary Primitives (Prim1)
           ;;;; Arithmetic primitives
           [(list 'add1 e)          (Prim1 'add1          (parse e))]
           [(list 'sub1 e)          (Prim1 'sub1          (parse e))]

           ;;;; Predicates
           [(list 'zero? e)         (Prim1 'zero?         (parse e))]
           [(list 'char? e)         (Prim1 'char?         (parse e))]
           [(list 'eof-object? e)   (Prim1 'eof-object?   (parse e))]

           ;;;; Value conversions
           [(list 'integer->char e) (Prim1 'integer->char (parse e))]
           [(list 'char->integer e) (Prim1 'char->integer (parse e))]

           ;;;; I/O, Effects
           [(list 'write-byte e)    (Prim1 'write-byte    (parse e))]

           ;;;; Inductive data
           [(list 'quote (list))    (Empty)]
           [(list 'box e)           (Prim1 'box   (parse e))]
           [(list 'unbox e)         (Prim1 'unbox (parse e))]
           [(list 'car e)           (Prim1 'car   (parse e))]
           [(list 'cdr e)           (Prim1 'cdr   (parse e))]

           ;; Binary Primitives (Prim2)
           [(list '+ e1 e2)     (Prim2 '+ (parse e1) (parse e2))]
           [(list '- e1 e2)     (Prim2 '- (parse e1) (parse e2))]
           [(list 'cons e1 e2)  (Prim2 'cons (parse e1) (parse e2))]

           ;; Control/Sequencing operators
           [(list 'begin e1 e2)
            (Begin (parse e1) (parse e2))]
           [(list 'if e1 e2 e3)
            (If (parse e1) (parse e2) (parse e3))]

           ;; Local binding
           [(list 'let (list (list (? symbol? x) v)) e)
            (Let x (parse v) (parse e))]

           ;; Error
           [x (error "Parsing error, found: " x)])]))
