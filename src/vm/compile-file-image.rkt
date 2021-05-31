#lang racket
(provide main)
(require "../parse.rkt" "compile-vm.rkt" "../mparse.rkt" "ast.rkt" a86)

;; main: String -> Void
;; So, in summary:
;; 1. String -> S-exps (Reader)
;; 2. S-exps -> AST (parse)
;; 3. AST -> a86 AST (compile)
;; Result: a list of vectors
(define (main filename)
  (let ([filename-port (open-input-file filename)])
    (begin
      (print
        (compile
         (parse
          (mread (port->string filename-port) filename))))
      (close-input-port filename-port))))
