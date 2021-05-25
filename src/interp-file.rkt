#lang racket
(provide main)
(require "parse.rkt" "interp.rkt")

;; main: String -> Void
;; main takes a String representation of our Lonnrot program
;; and evaluates it
;;
;; There are two parsing stages:
;; 1. From the string to S-exps (done by the #lang racket reader)
;; 2. From the S-exps to the AST (done by `parse`)
;;
;; So this interpreter actually interprets the AST representation of
;; the Lonnrot program
(define (main filename)
  (let ([program-port (open-input-file filename)])
    (begin
      (read-line program-port)
      (displayln (interp (parse (read program-port))))
      (close-input-port program-port))))
