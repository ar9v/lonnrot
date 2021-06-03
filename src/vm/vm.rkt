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

(define VM 0)

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

    (set! VM (vm
              0                                                                  ;; ip
              (hash 'eq #f 'lt #f 'gt #f)                                        ;; Flags
              (hash 'rax 0 'rbx 0 'rcx 0 'rdx 0 'r8  0 'r9  0 'rdi 0 'rsp 79999) ;; registers
              code                                                               ;; [Instruction]
              (build-list 80000 (lambda (x) 0))                                  ;; Memory
              label-map                                                          ;; <Symbol, Integer (Address)>
              halt))
    (run)))                                                           ;; Int

;; run: VM -> Answer
;; run takes a vm struct, which represents a state at a given time
;; and executes the current code until the final Ret instruction has
;; been found
(define (run)
  (cond [(= (vm-ip VM) (vm-halt VM))
         (print_result (hash-ref (vm-regs VM) 'rax))]
        [else
         (op-dispatch)
         (run)]))

;; op-dispatch: VM -> VM
;; op-dispatch takes a vm (i.e. a state in the running code) and depending
;; on the operation pointed to by ip, modifies the vm's state and returns
;; the new state
(define (op-dispatch)
  (let ([op (load-op)])
    (match op
      [(? Extern? op)
       (set-vm-ip! VM (add1 (vm-ip VM)))]
      [(Label l)
       (set-vm-ip! VM (add1 (vm-ip VM)))]

      [(? Mov? op)
       (mov op)]

      [(? Or? op)
       (bit-or op)]
      [(? And? op)
       (bit-and op)]
      [(? Xor? op)
       (bit-xor op)]

      [(? Sal? op)
       (sal op)]
      [(? Sar? op)
       (sar op)]

      [(? Cmp? op)
       (cmp op)]

      [(? Je? op)
       (je op)]
      [(? Jne? op)
       (jne op)]
      [(? Jl? op)
       (jl op)]
      [(? Jg? op)
       (jg op)]
      [(? Jmp? op)
       (jmp op)]

      [(? Add? op)
       (asm-add op)]
      [(? Sub? op)
       (asm-sub op)]

      [(? Call? op)
       (call op)]

      [(? Ret? op)
       (ret op)]

      [(Push _)
       (push op)]
      [(Pop _)
       (pop op)]

      [(Lea _ _)
       (lea op)]
      [(Ret)
       (set-vm-ip! VM (add1 (vm-ip VM)))])))

;; load-op: VM -> Instruction
;; load-op takes a vm and, using its current ip, produces
;; the vector that corresponds to the current instruction.
(define (load-op)
  (list-ref (vm-code VM)
            (vm-ip VM)))


;; MEMORY MANIPULATION
;; fetch-offset: Reg x Offset x VM -> Address
;; fetch-offset takes a register, whose value should be an address,
;; an offset, and a VM and returns the resulting memory location
;;
;; If we point to rsp, then we want to look upwards, i.e. to stuff
;; behind in the stack, if we point to the heap, we want to look downards
;; TODO: how do we tell when we are offsetting rax, etc.?
(define (fetch-offset reg off)
  (+ (reg->val reg) off))

;; deref-ptr: Address x VM -> Value
(define (deref-ptr p)
  (let ([mem (vm-mem VM)])
    (list-ref mem p)))

;; load-to-mem: Int x Address x VM -> VM
;; load-to-mem takes an integer, an address and a vm and returns a
;; vm with updated state
(define (load-to-mem v p)
  (let ([mem (vm-mem VM)])
    (set-vm-mem! VM (list-set mem p v))))

;; ASM INSTRUCTIONS
;; mov: Op x VM -> VM
;; mov takes a vector, an op, and a VM, and returns
;; a VM where register `x` in #(Op x y) has been set
;; to value y
;;
;; Mov takes two arguments, which may be
;; arg 1 - A register or an offset
;; arg 2 - A register, an offset or an integer
(define (mov op)
  (let ([regs (vm-regs VM)])
    (match op

      ;; Destination is a register
      [(Mov (? register? dst) src)
       (set-vm-regs! VM (hash-set regs dst (operand->value src)))]

      ;; Destination is an offset
      [(Mov (Offset dst o) src)
       (load-to-mem (operand->value src) (fetch-offset dst o))]

      [_ (error "Invalid MOV operation" op)])

    (set-vm-ip! VM (add1 (vm-ip VM)))))

;; lea: Op x VM -> VM
;; lea will take op (Lea dst label) and fetch `label`s address and place it
;; in dst. NOTE this address is different from a `ptr`, it is simply the
;; integer at which a given label is found
;;
;; dst: Either Register Offset
;; label: symbol
(define (lea op)
  (let* ([regs (vm-regs VM)]
         [label-addrs (vm-label-map VM)]
         [dst-addr (hash-ref label-addrs (Lea-x op))])
    (match (Lea-dst op)
      [(? register? dst)
       (set-vm-regs! VM (hash-set regs dst dst-addr))]

      [(Offset dst o)
       (load-to-mem dst-addr (fetch-offset dst o))])

    (set-vm-ip! VM (add1 (vm-ip VM)))))


(define (bit-op f op)
  (let ([regs (vm-regs VM)])

    (match op
      [(cons (? register? dst) src)
       (set-vm-regs!
        VM
        (hash-set regs dst (f (operand->value dst) (operand->value src))))]

      ;; NOTE: In our compiler we never produce an offset destination
      [_ (error "Invalid bitwise operation" op)])

    (set-vm-ip! VM (add1 (vm-ip VM)))))

(define (bit-or op) (bit-op bitwise-ior (cons (Or-dst op) (Or-src op))))
(define (bit-and op) (bit-op bitwise-and (cons (And-dst op) (And-src op))))
(define (bit-xor op) (bit-op bitwise-xor (cons (Xor-dst op) (Xor-src op))))


(define (push op)
  (let ([mem (vm-mem VM)]
        [regs (vm-regs VM)])

    ;; Decrement rsp
    (set-vm-regs!
     VM
     (hash-set regs 'rsp (- (hash-ref regs 'rsp) 8)))

    (load-to-mem (operand->value (Push-a1 op))
                 (fetch-offset 'rsp 0))

    ;; Increment ip
    (set-vm-ip! VM (add1 (vm-ip VM)))))

(define (pop op)
  (let ([regs (vm-regs VM)])

    ;; First, we fetch the value from the stack
    (set-vm-regs!
     VM
     (hash-set regs (Pop-a1 op) (operand->value (Offset 'rsp 0))))

    ;; Then we increment (i.e. shrink) the stack pointer
    (set-vm-regs!
     VM
     (hash-set (vm-regs VM) 'rsp (+ (operand->value 'rsp) 8)))

    ;; Finally, we increment the ip
    (set-vm-ip! VM (add1 (vm-ip VM)))))

(define (cmp op)
  (let ([dst (Cmp-a1 op)]
        [src (Cmp-a2 op)])
    (set-flags (operand->value dst)
               (operand->value src))

   (set-vm-ip! VM (add1 (vm-ip VM)))))

(define (set-flags a1 a2)
  (cond [(< a1 a2)
         (set-vm-flags! VM (hash 'eq #f 'lt #t 'gt #f))]
        [(= a1 a2)
         (set-vm-flags! VM (hash 'eq #t 'lt #f 'gt #f))]
        [else
         (set-vm-flags! VM (hash 'eq #f 'lt #f 'gt #t))]))


(define (jmp op)
  ;; We check if Jmp's arg is a register, if it is we get its address
  (match (hash-ref (vm-label-map VM) (Jmp-x op))
    [(? integer? addr)
     (set-vm-ip! VM addr)]

    ;; It's a runtime operation
    [form (eval form ns)]))

(define (jl op)
  (if (get-flag 'lt)
      (jmp (Jmp (Jl-x op)))
      (set-vm-ip! VM (add1 (vm-ip VM)))))

(define (je op)
  (if (get-flag 'eq)
      (jmp (Jmp (Je-x op)))
      (set-vm-ip! VM (add1 (vm-ip VM)))))

(define (jg op)
  (if (get-flag 'gt)
      (jmp (Jmp (Jg-x op)))
      (set-vm-ip! VM (add1 (vm-ip VM)))))

(define (jne op)
  (if (not (get-flag 'eq))
      (jmp (Jmp (Jne-x op)))
      (set-vm-ip! VM (add1 (vm-ip VM)))))


(define (asm-add op)
  (let ([regs (vm-regs VM)]
        [dst (Add-dst op)])
    (set-vm-regs!
     VM
     (hash-set
      regs dst
      (+ (operand->value dst) (operand->value (Add-src op)))))

    (set-vm-ip! VM (add1 (vm-ip VM)))))


(define (asm-sub op)
 (let ([regs (vm-regs VM)]
        [dst (Sub-dst op)])
    (set-vm-regs!
     VM
     (hash-set
      regs dst
      (- (operand->value dst) (operand->value (Sub-src op)))))

    (set-vm-ip! VM (add1 (vm-ip VM)))))


;; Calls
(define (call op)
  (let ([ret-label (string->symbol (string-append (symbol->string (Call-x op)) "ret"))])
    ;; If we are calling from a register, we should create a
    ;; mapping from that register to the address it holds
    (when (register? (Call-x op))
      (set-vm-label-map! VM
                         (hash-set (vm-label-map VM) (Call-x op) (operand->value (Call-x op)))))

    ;; Stick the return symbol with the address in the symbol map
    (set-vm-label-map!
     VM
     (hash-set (vm-label-map VM) ret-label (vm-ip VM)))

    ;; Load the current code address onto rax
    ;; We don't use `lea` to avoid moving ip
    (set-vm-regs!
     VM
     (hash-set (vm-regs VM) 'rax (vm-ip VM)))

    ;; Push that onto the stack
    (push (Push 'rax))
    (set-vm-ip! VM (sub1 (vm-ip VM))) ;; Again, keep ip consistent

    (jmp (Jmp (Call-x op)))))

(define (ret op)
  ;; Pop the stack
  (pop (Pop 'rbx))
  (set-vm-ip! VM (sub1 (vm-ip VM)))

  ;; As in `call`, create a mapping of the register in the label-map
  ;; for rbx
  (set-vm-label-map! VM (hash-set (vm-label-map VM)
                                  'rbx (add1 (operand->value 'rbx))))

  (jmp (Jmp 'rbx)))

;; Bit twiddling
(define (sal op)
  (let ([reg (Sal-dst op)])
    (set-vm-regs!
     VM
     (hash-set (vm-regs VM) reg (arithmetic-shift (operand->value reg) (Sal-i op))))

    (set-vm-ip! VM (add1 (vm-ip VM)))))

(define (sar op)
  (let ([reg (Sal-dst op)])
    (set-vm-regs!
     VM
     (hash-set (vm-regs VM) reg (arithmetic-shift (operand->value reg) (- (Sal-i op)))))

    (set-vm-ip! VM (add1 (vm-ip VM)))))

;; Helpers
(define (operand->value op)
  (match op
    [(? register? op) (reg->val op)]
    [(Offset r o)     (deref-ptr (fetch-offset r o))]
    [(? integer? v)   v]
    [_ (error "Invalid operand" op)]))

(define (reg->val reg)
  (hash-ref
   (vm-regs VM)
   reg))

(define (get-flag s)
  (hash-ref (vm-flags VM) s))

;; RUNTIME
(define (raise_error)
  (error "VM Crash"))

(define (print_result res)
  (cond [(bit-cons? res) (print_cons res)]
        [(bit-box? res)
         (box (bits->value (deref-ptr (bitwise-xor res type-box))))]
        [(bit-f? res) '<PROC>]
        [(bit-string? res) (list->string (print_string res))]
        [(bit-int? res)
         (bits->value res)]
        [(bit-char? res)
         (let ([codepoint (bits->value res)])
           (integer->char codepoint))]
        [(val-true? res) #t]
        [(val-false? res) #f]
        [(= res val-void) (void)]
        [(= res val-empty) '()]))

(define (print_cons res)
  (let ([head (deref-ptr (bitwise-xor (+ 8 res) type-cons))]
        [tail (deref-ptr (bitwise-xor res type-cons))])
    (cons (print_result head)
          (cond [(= tail val-empty) '()]
                [(bit-cons? tail) (print_cons tail)]
                [else (bits->value tail)]))))

(define (print_string res)
  (let* ([str-ptr (deref-ptr (bitwise-xor res type-string))]
         [len (bits->value str-ptr)])

    (for/list ([i (range len)])
      (print_result (deref-ptr (bitwise-xor (+ res (* 8 (add1 i))) type-string))))))
