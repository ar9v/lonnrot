#lang racket
(provide interp/io)
(require "interp.rkt")

;; interp/io: Expr x String -> (Cons Value String)
;;
;; (interp/io e input) takes an expression e (again, represented
;; in AST form) that represents an IO operation and evaluates it
;; with a string, input
;;
;; The results of evaluating the expression are then collected
;; in a pair, which encode the resulting value (car) and the
;; state of the port operated on (cdr)
(define (interp/io e input)
  (parameterize ([current-output-port (open-output-string)]
                 [current-input-port (open-input-string input)])
    (cons (interp e)
          (get-output-string (current-output-port)))))
