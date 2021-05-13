#lang racket

(let ((x (cons 1 (cons 2 (cons 3 '())))))
  (- 4
     (+ (car x)
        (car (cdr x)))))
