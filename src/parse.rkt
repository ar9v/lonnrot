#lang racket
(provide parse)
(require "ast.rkt")

;; parse: Sexp -> Expr
(define (parse s)
  (cond [(integer? s) (Int s)]
        [else (error "Parsing error")]))
