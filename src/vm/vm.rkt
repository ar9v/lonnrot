#lang racket
(provide main)
(require "qast.rkt" racket/struct "types.rkt")
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(struct vm
  (ip flags regs code mem label-map halt)
  #:mutable

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'vm)
      (lambda (obj) (list (vm-ip obj)
                          (vm-flags obj)
                          (vm-regs obj)
                          (vm-code obj)
                          (vm-mem obj)
                          (vm-label-map obj)
                          (vm-halt obj)))))])

;; get-halt: [#(Op _ ...)] x Int -> Int
;; get-halt takes a list of ops and an integer which represents
;; an accumulator; this will tell us where is the return for
;; 'entry
(define (get-halt code ip)
  (match (car code)
    [(Ret) ip]
    [_ (get-halt (cdr code) (add1 ip))]))

(define (get-addr-labels code addr-hash ip)
  (match code
    ['() addr-hash]
    [_
     (match (car code)
       [(Label l)
        (get-addr-labels (cdr code) (hash-set addr-hash l ip) (add1 ip))]
       [(Extern l)
        (get-addr-labels (cdr code) (hash-set addr-hash l (list l)) (add1 ip))]
       [_ (get-addr-labels (cdr code) addr-hash (add1 ip))])]))

;; main: String -> Answer
;; Main reads an image file, which is a list of a86 ops
;; and calls the VM's run function
(define (main image-file)
  (let* ([filename-port (open-input-file image-file)]
         [code (eval (read filename-port) ns)]
         [label-map (get-addr-labels code (hash) 0)]
         [halt (get-halt code 0)])
    (run
     (vm
      0                                                                  ;; ip
      (hash 'eq #f 'lt #f 'gt #f)                                        ;; Flags
      (hash 'rax 0 'rbx 0 'rcx 0 'rdx 0 'r8  0 'r9  0 'rdi 0 'rsp 79999) ;; registers
      code                                                               ;; [Instruction]
      (build-list 80000 (lambda (x) 0))                                  ;; Memory
      label-map                                                          ;; <Symbol, Integer (Address)>
      halt))))                                                           ;; Int

;; run: VM -> Answer
;; run takes a vm struct, which represents a state at a given time
;; and executes the current code until the final Ret instruction has
;; been found
(define (run vm)
  (cond [(= (vm-ip vm) (vm-halt vm))
         (print_result (hash-ref (vm-regs vm) 'rax) vm)]
        [else
         (op-dispatch vm)
         (run vm)]))

;; op-dispatch: VM -> VM
;; op-dispatch takes a vm (i.e. a state in the running code) and depending
;; on the operation pointed to by ip, modifies the vm's state and returns
;; the new state
(define (op-dispatch vm)
  (let ([op (load-op vm)])
    (displayln op)
    (displayln (vm-ip vm))
    (displayln (vm-regs vm))
    (match op
      [(? Extern? op)
       (set-vm-ip! vm (add1 (vm-ip vm))) vm]
      [(Label l)
       (set-vm-ip! vm (add1 (vm-ip vm))) vm]

      [(? Mov? op)
       (mov op vm)]

      [(? Or? op)
       (bit-or op vm)]
      [(? And? op)
       (bit-and op vm)]
      [(? Xor? op)
       (bit-xor op vm)]

      [(? Sal? op)
       (sal op vm)]
      [(? Sar? op)
       (sar op vm)]

      [(? Cmp? op)
       (cmp op vm)]

      [(? Je? op)
       (je op vm)]
      [(? Jne? op)
       (jne op vm)]
      [(? Jl? op)
       (jl op vm)]
      [(? Jg? op)
       (jg op vm)]
      [(? Jmp? op)
       (jmp op vm)]

      [(? Add? op)
       (asm-add op vm)]
      [(? Sub? op)
       (asm-sub op vm)]

      [(? Call? op)
       (call op vm)]

      [(? Ret? op)
       (ret op vm)]

      [(Push _)
       (push op vm)]
      [(Pop _)
       (pop op vm)]

      [(Lea _ _)
       (lea op vm)]
      [(Ret)
       (set-vm-ip! vm (add1 (vm-ip vm))) vm])))

;; load-op: VM -> Instruction
;; load-op takes a vm and, using its current ip, produces
;; the vector that corresponds to the current instruction.
(define (load-op vm)
  (list-ref (vm-code vm)
            (vm-ip vm)))


;; MEMORY MANIPULATION
;; fetch-offset: Reg x Offset x VM -> Address
;; fetch-offset takes a register, whose value should be an address,
;; an offset, and a VM and returns the resulting memory location
;;
;; If we point to rsp, then we want to look upwards, i.e. to stuff
;; behind in the stack, if we point to the heap, we want to look downards
;; TODO: how do we tell when we are offsetting rax, etc.?
(define (fetch-offset reg off vm)
  (+ (reg->val reg vm) off))

;; deref-ptr: Address x VM -> Value
(define (deref-ptr p vm)
  (let ([mem (vm-mem vm)])
    (list-ref mem p)))

;; load-to-mem: Int x Address x VM -> VM
;; load-to-mem takes an integer, an address and a vm and returns a
;; vm with updated state
(define (load-to-mem v p vm)
  (let ([mem (vm-mem vm)])
    (set-vm-mem! vm (list-set mem p v))
    vm))

;; ASM INSTRUCTIONS
;; mov: Op x VM -> VM
;; mov takes a vector, an op, and a VM, and returns
;; a VM where register `x` in #(Op x y) has been set
;; to value y
;;
;; Mov takes two arguments, which may be
;; arg 1 - A register or an offset
;; arg 2 - A register, an offset or an integer
(define (mov op vm)
  (let ([regs (vm-regs vm)])
    (match op

      ;; Destination is a register
      [(Mov (? register? dst) src)
       (set-vm-regs! vm (hash-set regs dst (operand->value src vm)))]

      ;; Destination is an offset
      [(Mov (Offset dst o) src)
       (load-to-mem (operand->value src vm) (fetch-offset dst o vm) vm)]

      [_ (error "Invalid MOV operation" op)])

    (set-vm-ip! vm (add1 (vm-ip vm)))))

;; lea: Op x VM -> VM
;; lea will take op (Lea dst label) and fetch `label`s address and place it
;; in dst. NOTE this address is different from a `ptr`, it is simply the
;; integer at which a given label is found
;;
;; dst: Either Register Offset
;; label: symbol
(define (lea op vm)
  (let* ([regs (vm-regs vm)]
         [label-addrs (vm-label-map vm)]
         [dst-addr (hash-ref label-addrs (Lea-x op))])
    (match (Lea-dst op)
      [(? register? dst)
       (set-vm-regs! vm (hash-set regs dst dst-addr))]

      [(Offset dst o)
       (load-to-mem dst-addr (fetch-offset dst o vm) vm)])

    (set-vm-ip! vm (add1 (vm-ip vm)))))


(define (bit-op f op vm)
  (let ([regs (vm-regs vm)])

    (match op
      [(cons (? register? dst) src)
       (set-vm-regs!
        vm
        (hash-set regs dst (f (operand->value dst vm) (operand->value src vm))))]

      ;; NOTE: In our compiler we never produce an offset destination
      [_ (error "Invalid bitwise operation" op)])

    (set-vm-ip! vm (add1 (vm-ip vm)))))

(define (bit-or op vm) (bit-op bitwise-ior (cons (Or-dst op) (Or-src op)) vm))
(define (bit-and op vm) (bit-op bitwise-and (cons (And-dst op) (And-src op)) vm))
(define (bit-xor op vm) (bit-op bitwise-xor (cons (Xor-dst op) (Xor-src op)) vm))


(define (push op vm)
  (let ([mem (vm-mem vm)]
        [regs (vm-regs vm)])

    ;; Decrement rsp
    (set-vm-regs!
     vm
     (hash-set regs 'rsp (- (hash-ref regs 'rsp) 8)))

    (load-to-mem (operand->value (Push-a1 op) vm)
                 (fetch-offset 'rsp 0 vm)
                 vm)

    ;; Increment ip
    (set-vm-ip! vm (add1 (vm-ip vm)))
    vm))

(define (pop op vm)
  (let ([regs (vm-regs vm)])

    ;; First, we fetch the value from the stack
    (set-vm-regs!
     vm
     (hash-set regs (Pop-a1 op) (operand->value (Offset 'rsp 0) vm)))

    ;; Then we increment (i.e. shrink) the stack pointer
    (set-vm-regs!
     vm
     (hash-set (vm-regs vm) 'rsp (+ (operand->value 'rsp vm) 8)))

    ;; Finally, we increment the ip
    (set-vm-ip! vm (add1 (vm-ip vm)))
    vm))

(define (cmp op vm)
  (let ([dst (Cmp-a1 op)]
        [src (Cmp-a2 op)])
    (set-flags (operand->value dst vm)
               (operand->value src vm)
               vm)

   (set-vm-ip! vm (add1 (vm-ip vm)))))

(define (set-flags a1 a2 vm)
  (cond [(< a1 a2)
         (set-vm-flags! vm (hash 'eq #f 'lt #t 'gt #f))]
        [(= a1 a2)
         (set-vm-flags! vm (hash 'eq #t 'lt #f 'gt #f))]
        [else
         (set-vm-flags! vm (hash 'eq #f 'lt #f 'gt #t))]))


(define (jmp op vm)
  ;; We check if Jmp's arg is a register, if it is we get its address
  (match (hash-ref (vm-label-map vm) (Jmp-x op))
    [(? integer? addr)
     (set-vm-ip! vm addr)]

    ;; It's a runtime operation
    [form (eval form ns)]))

(define (jl op vm)
  (if (get-flag vm 'lt)
      (jmp (Jmp (Jl-x op)) vm)
      (set-vm-ip! vm (add1 (vm-ip vm)))))

(define (je op vm)
  (if (get-flag vm 'eq)
      (jmp (Jmp (Je-x op)) vm)
      (set-vm-ip! vm (add1 (vm-ip vm)))))

(define (jg op vm)
  (if (get-flag vm 'gt)
      (jmp (Jmp (Jg-x op)) vm)
      (set-vm-ip! vm (add1 (vm-ip vm)))))

(define (jne op vm)
  (if (not (get-flag vm 'eq))
      (jmp (Jmp (Jne-x op)) vm)
      (set-vm-ip! vm (add1 (vm-ip vm)))))


(define (asm-add op vm)
  (let ([regs (vm-regs vm)]
        [dst (Add-dst op)])
    (set-vm-regs!
     vm
     (hash-set
      regs dst
      (+ (operand->value dst vm) (operand->value (Add-src op) vm))))

    (set-vm-ip! vm (add1 (vm-ip vm)))))


(define (asm-sub op vm)
 (let ([regs (vm-regs vm)]
        [dst (Sub-dst op)])
    (set-vm-regs!
     vm
     (hash-set
      regs dst
      (- (operand->value dst vm) (operand->value (Sub-src op) vm))))

    (set-vm-ip! vm (add1 (vm-ip vm)))))


;; Calls
(define (call op vm)
  (let ([ret-label (string->symbol (string-append (symbol->string (Call-x op)) "ret"))])
    ;; If we are calling from a register, we should create a
    ;; mapping from that register to the address it holds
    (when (register? (Call-x op))
      (set-vm-label-map! vm
                         (hash-set (vm-label-map vm) (Call-x op) (operand->value (Call-x op) vm))))

    ;; Stick the return symbol with the address in the symbol map
    (set-vm-label-map!
     vm
     (hash-set (vm-label-map vm) ret-label (vm-ip vm)))

    ;; Load the current code address onto rax
    ;; We don't use `lea` to avoid moving ip
    (set-vm-regs!
     vm
     (hash-set (vm-regs vm) 'rax (vm-ip vm)))

    ;; Push that onto the stack
    (push (Push 'rax) vm)
    (set-vm-ip! vm (sub1 (vm-ip vm))) ;; Again, keep ip consistent

    (jmp (Jmp (Call-x op)) vm)))

(define (ret op vm)
  ;; Pop the stack
  (pop (Pop 'rbx) vm)
  (set-vm-ip! vm (sub1 (vm-ip vm)))

  ;; As in `call`, create a mapping of the register in the label-map
  ;; for rbx
  (set-vm-label-map! vm (hash-set (vm-label-map vm)
                                  'rbx (add1 (operand->value 'rbx vm))))

  (jmp (Jmp 'rbx) vm))

;; Bit twiddling
(define (sal op vm)
  (let ([reg (Sal-dst op)])
    (set-vm-regs!
     vm
     (hash-set (vm-regs vm) reg (arithmetic-shift (operand->value reg vm) (Sal-i op))))

    (set-vm-ip! vm (add1 (vm-ip vm)))))

(define (sar op vm)
  (let ([reg (Sal-dst op)])
    (set-vm-regs!
     vm
     (hash-set (vm-regs vm) reg (arithmetic-shift (operand->value reg vm) (- (Sal-i op)))))

    (set-vm-ip! vm (add1 (vm-ip vm)))))

;; Helpers
(define (operand->value op vm)
  (match op
    [(? register? op) (reg->val op vm)]
    [(Offset r o)     (deref-ptr (fetch-offset r o vm) vm)]
    [(? integer? v)   v]
    [_ (error "Invalid operand" op)]))

(define (reg->val reg vm)
  (hash-ref
   (vm-regs vm)
   reg))

(define (get-flag vm s)
  (hash-ref (vm-flags vm) s))

;; RUNTIME
(define (raise_error)
  (error "VM Crash"))

(define (print_result res vm)
  (display res)
  (cond [(bit-cons? res) (print_cons res vm)]
        [(bit-box? res)
         (box (bits->value (deref-ptr (bitwise-xor res type-box) vm)))]
        [(bit-f? res) '<PROC>]
        [(bit-string? res) (list->string (print_string res vm))]
        [(bit-int? res)
         (bits->value res)]
        [(bit-char? res)
         (let ([codepoint (bits->value res)])
           (integer->char codepoint))]
        [(val-true? res) #t]
        [(val-false? res) #f]
        [(= res val-void) (void)]
        [(= res val-empty) '()]))

(define (print_cons res vm)
  (let ([head (deref-ptr (bitwise-xor (+ 8 res) type-cons) vm)]
        [tail (deref-ptr (bitwise-xor res type-cons) vm)])
    (cons (print_result head vm)
          (cond [(= tail val-empty) '()]
                [(bit-cons? tail) (print_cons tail vm)]
                [else (bits->value tail)]))))

(define (print_string res vm)
  (let* ([str-ptr (deref-ptr (bitwise-xor res type-string) vm)]
         [len (bits->value str-ptr)])

    (for/list ([i (range len)])
      (print_result (deref-ptr (bitwise-xor (+ res (* 8 (add1 i))) type-string) vm) vm))))
