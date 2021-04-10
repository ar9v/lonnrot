#lang racket
(provide interp)
(require "ast.rkt")

(define (interp e)
  (match e
    [(Int i) i]))
