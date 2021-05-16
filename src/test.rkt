#lang racket

(begin
  (define (id z) z)
  (define (f y) (+ y y))
  (define (sum acc n) (let ((x 42))
                        (if (zero? n)
                          acc
                          (sum (+ n acc) (sub1 n)))))

  (let ((x 42))
    (let ((y 84))
      (sum 0 100000))))
