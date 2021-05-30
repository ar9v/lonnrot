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

(struct vm
  (ip sp regs code s h)
  #:mutable)

;; main: String -> Answer
;; Main reads an image file, which is a list of a86 ops
;; and calls the VM's run function
(define (main image-file)
  (let ([filename-port (open-input-file image-file)])
    (run
     (vm
      0
      1
      (make-hash '((rax . 0) (rbx . 0) (rcx . 0) (rdx . 0) (r8 . 0) (r9 . 0) (rdi . 0)))
      (read filename-port)
      '()
      '()))))

;; run: VM -> Answer
;; run takes a vm struct, which represents a state at a given time
;; and executes the current code until the final Ret instruction has
;; been found
(define (run vm)
  (cond [(empty? (vm-code vm)) "finished"]
        [else
         (displayln (load-op (car (vm-code vm))))
         (set-vm-code! vm (cdr (vm-code vm)))
         (run vm)]))

(define (load-op ins)
  (vector-ref ins 0))


;; (Push x)
;; -> Decrements rsp (Sub rsp 8)
;; -> Moves `x` to the top frame (Mov (Offset rsp 0) x)
;;
;; (Pop x)
;; -> Moves what is pointed to by rsp onto x (Mov x (Offset rsp 0))
;; -> Increments rsp (Add rsp 8)
;;
;; (Call f)
;; -> push the return address (i.e. where we left off)
;; -> jumps to label pointed to by f
;;
;; (Ret)
;; -> pops the return address from the stack
;; -> jumps to this address
