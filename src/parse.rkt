#lang racket
(provide parse)
(require "ast.rkt")

;; For Iniquity, we use the same trick that we use for the
;; interpreter: We wrap the call to our original `parse` function
;; with the pattern-match that produces the Prog struct
(define (parse s)
  (match s
    [(list 'begin
           (and ds (list 'define _ _)) ...
           e)
     (Prog (map parse-define ds) (parse-e e))]

    [e (Prog '() (parse-e e))]))


;; parse-define takes an s-exp that represents a function definition
;; and parses it into its AST representation
(define (parse-define defn)
  (match defn
    [(list 'define
           (list (? symbol? fn) (? symbol? params) ...)
           body)
     (Defn fn params (parse-e body))]

    [_ (error "Error parsing function definition" defn)]))


;; parse-bindings: [[Symbol Expr]] -> [[Symbol AST]]
(define (parse-bindings bs)
  (match bs
    ['() '()]
    [(cons (list (? symbol? sym) expr) rest)
     (cons (list sym (parse-e expr))
           (parse-bindings rest))]))

;; parse: Sexp -> Maybe Error
;;
;; (parse s) takes a symbolic expression, s, which
;; represents a Lonnrot Scheme program, and turns it
;; into AST form (a form which is defined in `ast.rkt`)
;;
;; The symbolic expression s represents the program parsed
;; by the Racket reader.
(define (parse-e s)
  ;; We either have a value or a primitive application
  ;; We use cond here because we want to evaluate the predicates
  ;; as opposed to checking for a particular structure (i.e. `match`)
  (cond [(integer? s)   (Int s)]
        [(boolean? s)   (Bool s)]
        [(char? s)      (Char s)]
        [(empty? s)     (Empty)]
        [else
         (match s
           ;; Nullary primitives (Prim0)
           ;; We put eof first because it overlaps with symbol
           ['eof                    (Eof)]
           [(? symbol? x)           (Var x)]
           [(list 'read-byte)       (Prim0 'read-byte)]
           [(list 'peek-byte)       (Prim0 'peek-byte)]
           [(list 'void)            (Prim0 'void)]

           ;; Unary Primitives (Prim1)
           ;;;; Arithmetic primitives
           [(list 'add1 e)          (Prim1 'add1          (parse-e e))]
           [(list 'sub1 e)          (Prim1 'sub1          (parse-e e))]

           ;;;; Predicates
           [(list 'zero? e)         (Prim1 'zero?         (parse-e e))]
           [(list 'char? e)         (Prim1 'char?         (parse-e e))]
           [(list 'eof-object? e)   (Prim1 'eof-object?   (parse-e e))]

           ;;;; Value conversions
           [(list 'integer->char e) (Prim1 'integer->char (parse-e e))]
           [(list 'char->integer e) (Prim1 'char->integer (parse-e e))]

           ;;;; I/O, Effects
           [(list 'write-byte e)    (Prim1 'write-byte    (parse-e e))]

           ;;;; Inductive data
           [(list 'quote (list))    (Empty)]
           [''()                    (Empty)]
           [(list 'box e)           (Prim1 'box   (parse-e e))]
           [(list 'unbox e)         (Prim1 'unbox (parse-e e))]
           [(list 'car e)           (Prim1 'car   (parse-e e))]
           [(list 'cdr e)           (Prim1 'cdr   (parse-e e))]

           ;; Binary Primitives (Prim2)
           [(list '+ e1 e2)     (Prim2 '+ (parse-e e1) (parse-e e2))]
           [(list '- e1 e2)     (Prim2 '- (parse-e e1) (parse-e e2))]
           ;; TODO: implement, probably will be part of std lib (there's no quad for multiplication)
           ;; [(list '* e1 e2)     (Prim2 '* (parse-e e1) (parse-e e2))]

           [(list '< e1 e2)     (Prim2 '< (parse-e e1) (parse-e e2))]
           [(list '> e1 e2)     (Prim2 '> (parse-e e1) (parse-e e2))]
           [(list '= e1 e2)     (Prim2 '= (parse-e e1) (parse-e e2))]

           [(list 'cons e1 e2)  (Prim2 'cons (parse-e e1) (parse-e e2))]
           [(list 'eq?   e1 e2)  (Prim2 'eq?  (parse-e e1) (parse-e e2))]

           ;; Control/Sequencing operators
           [(list 'begin args ...)
            (Begin (map parse-e args))]
           [(list 'if e1 e2 e3)
            (If (parse-e e1) (parse-e e2) (parse-e e3))]

           ;; Local binding
           [(list 'let (list (list (? symbol? x) v)) e)
            (Let x (parse-e v) (parse-e e))]

           [(list 'letrec bindings body)
            (LetRec (parse-bindings bindings) (parse-e body))]

           [(list 'lambda (? symbol-list? params) body)
            (Lambda '() params (parse-e body))]

           ;; Function definition and application
           [(cons e args)
            (App (parse-e e) (map parse-e args))]

           ;; Error
           [x (error "Parsing error, found: " x)])]))


;; symbol-list?: [Any] -> Boolean
;; symbol-list takes a list of atoms and returns true
;; if all of its atoms are symbols
(define (symbol-list? ls)
  (= 0 (length (filter (lambda (a) (not (symbol? a))) ls))))
