#lang racket
(provide run)
(require a86)

;; Data structures
;; The stack:
;; A list, this way we can perform lookups and so on
;;
;; Registers:
;; A hashmap, with the symbol representing the register
;; being accessed
;;
;; Flags:
;; For now we only need the EQ flag

(struct reg (val))
(struct vm (ip sp regs))


;; main: String -> Answer
;; Main reads an image file, which is a list of a86 ops
;; and calls the VM's run function
(define (main image-file)
  (let ([filename-port (open-input-file image-file)])
    (run (read filename-port)
         1)))


;; run: [ASM Ops] -> Answer
(define (run code pc)
  (cond [(empty? code) "Program finished successfully"]
        [else
         (op-dispatch (car code))
         (run (cdr code) pc)]))


(define (op-dispatch op)
  (match op
    [(list 'Extern s) (displayln "Found extern")]
    [(list 'Label l) (displayln "Found label")]
    [(list 'Mov r1 r2) (displayln "Found mov")]
    [_ (displayln "op does not exist")]))
