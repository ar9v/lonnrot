#lang racket
(provide run)
(require
 ;; a86
 "qast.rkt"
 racket/struct "types.rkt")
 ;;"qast.rkt") ;; TODO: eventually migrate structs to own homonymous structs

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

(struct ptr (type addr) #:prefab)

(struct vm
  (ip sp flags regs code s h label-map halt)
  #:mutable

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'vm)
      (lambda (obj) (list (vm-ip obj)
                          (vm-sp obj)
                          (vm-flags obj)
                          (vm-regs obj)
                          (vm-code obj)
                          (vm-s obj)
                          (vm-h obj)
                          (vm-label-map obj)
                          (vm-halt obj)))))])

(define test-file "test.obj")

;; getHalt: [#(Op _ ...)] x Int -> Int
;; getHalt takes a list of ops and an integer which represents
;; an accumulator; this will tell us where is the return for
;; 'entry
(define (get-halt code ip)
  (match (car code)
    [(Ret) ip]
    [_ (get-halt (cdr code) (add1 ip))]))

(define (get-addr-labels code addrHash ip)
  (match code
    ['() addrHash]
    [_
     (match (car code)
       [(Label l)
        (get-addr-labels (cdr code) (hash-set addrHash l ip) (add1 ip))]
       [_ (get-addr-labels (cdr code) addrHash (add1 ip))])]))

;; main: String -> Answer
;; Main reads an image file, which is a list of a86 ops
;; and calls the VM's run function
(define (main image-file)
  (let* ([filename-port (open-input-file image-file)]
         [code (eval (read filename-port))]
         [labelMap (get-addr-labels code (hash) 0)]
         [halt (get-halt code 0)])
    (run
     (vm
      0                                                                ;; ip
      1                                                                ;; stack pointer
      (hash 'eq #f 'lt #f 'gt #f)                                         ;; Flags
      (hash 'rax 0 'rbx 0 'rcx 0 'rdx 0 'r8  0 'r9  0 'rdi (ptr 'h 0)) ;; registers
      code                                                             ;; [Instruction]
      '()                                                              ;; Stack
      (build-list 8000 (lambda (x) 0))                                 ;; Heap
      labelMap                                                         ;; <Symbol, Integer (Address)>
      halt))))                                                         ;; Int

;; run: VM -> Answer
;; run takes a vm struct, which represents a state at a given time
;; and executes the current code until the final Ret instruction has
;; been found
(define (run vm)
  (cond [(= (vm-ip vm) (vm-halt vm))
         (bits->value (hash-ref (vm-regs vm) 'rax))]
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

;; fetch-offset: Reg x Offset x VM -> Address Pointer (Either (s . Int) | (h . Int))
;; fetch-offset takes a register, whose value should be an address,
;; an offset, and a VM and returns the given memory location, which
;; is a pair signaling which memory (stack or heap) and its address
(define (fetch-offset reg off vm)
  (let ([p (reg->val reg vm)])
    (ptr (ptr-type p)
         (+ off (ptr-addr p)))))

;; deref-ptr: (Sym . Int) x VM -> Value
(define (deref-ptr p vm)
  (match p
    [(ptr 's addr) (list-ref (vm-s vm) (ptr-addr p))]
    [(ptr 'h addr) (list-ref (vm-h vm) (ptr-addr p))]))

;; mov: Op x VM -> VM
;; mov takes a vector, an op, and a VM, and returns
;; a VM where register `x` in #(Op x y) has been set
;; to value y
;;
;; Mov takes two arguments, which may be
;; arg 1 - A register or an offset
;; arg 2 - A register, an offset or an integer
(define (mov op vm)
  (let ([regs (vm-regs vm)]
        [stack (vm-s vm)]
        [heap (vm-h vm)])
    (match op
      ;; Dst = register, Src = register
      [(Mov (? register? dst) (? register? src))
       (set-vm-regs! vm (hash-set regs dst (reg->val src vm)))]

      ;; Dst = register, Src = Offset
      [(Mov (? register? dst) (Offset src o))
       (set-vm-regs! vm (hash-set regs dst (deref-ptr (fetch-offset src o vm))))]

      ;; Dst = register, Src = Integer
      [(Mov (? register? dst) (? integer? v))
       (set-vm-regs! vm (hash-set regs dst v))]

      ;; Dst = Offset, Src = Register
      [(Mov (Offset dst o) (? register? src))
       (load-to-mem (reg->val src vm) (fetch-offset dst o vm) vm)]

      ;; Dst = Offset, Src = Integer
      [(Mov (Offset dst o) (? integer? v))
       (load-to-mem v (fetch-offset dst o vm) vm)]

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
  (let ([regs (vm-regs vm)]
        [labelAddrs (vm-label-map vm)])
    (match op
      [(Lea (? register? dst) s)
       (set-vm-regs! vm (hash-set regs dst (hash-ref labelAddrs s)))]

      [(Lea (Offset dst o) s)
       (load-to-mem (hash-ref labelAddrs s) (fetch-offset dst o vm) vm)])

    (set-vm-ip! vm (add1 (vm-ip vm)))))


(define (bit-op f op vm)
  (let ([regs (vm-regs vm)]
        [stack (vm-s vm)]
        [heap (vm-h vm)])

    (match op
      ;; Dst = register, Src = register
      [(cons (? register? dst) (? register? src))
       (set-vm-regs!
        vm
        (hash-set regs dst (f (reg->val dst vm) (reg->val src vm))))]

      ;; Dst = register, Src = Offset
      [(cons (? register? dst) (Offset src o))
       (set-vm-regs!
        vm
        (hash-set regs dst (f (reg->val dst vm) (deref-ptr (fetch-offset src o vm)))))]

      ;; Dst = register, Src = Integer
      [(cons (? register? dst) (? integer? v))
       (set-vm-regs! vm (hash-set regs dst (f (reg->val dst vm) v)))]

      ;; NOTE: In our compiler we never produce an offset destination
      [_ (error "Invalid bitwise operation" op)])

    (set-vm-ip! vm (add1 (vm-ip vm)))))

;; aor: Op x VM -> VM
;; aor takes (Or dst src) and returns a vm where dst is now
;; (bitwise-ior dst src)
(define (bit-or op vm)
  (bit-op bitwise-ior (cons (Or-dst op) (Or-src op)) vm))

(define (bit-and op vm)
  (bit-op bitwise-and (cons (And-dst op) (And-src op)) vm))

(define (bit-xor op vm)
  (bit-op bitwise-xor (cons (Xor-dst op) (Xor-src op)) vm))

(define (push op vm)
  (match op
    [(Push (? register? src))
     (set-vm-s! vm (cons (reg->val src vm) (vm-s vm)))
     (set-vm-sp! vm (add1 (vm-sp vm)))]

    [(Push (? integer? v))
     (set-vm-s! vm (cons v (vm-s vm)))
     (set-vm-sp! vm (add1 (vm-sp vm)))])

  (set-vm-ip! vm (add1 (vm-ip vm))))

(define (pop op vm)
  (let ([regs (vm-regs vm)])
    (match op
      [(Pop (? register? dst))
       (set-vm-regs! vm
                     (hash-set regs dst (car (vm-s vm))))
       (set-vm-s! vm (cdr (vm-s vm)))
       (set-vm-sp! vm (sub1 (vm-sp vm)))])

  (set-vm-ip! vm (add1 (vm-ip vm)))))

(define (cmp op vm)
  (match op
    [(Cmp (? register? r1) (? register? r2))
     (let ([a1 (reg->val r1 vm)]
           [a2 (reg->val r2 vm)])
       (set-flags a1 a2 vm))]

    [(Cmp (? register? r1) (Offset r2 o))
     (let ([a1 (reg->val r1 vm)]
           [a2 (deref-ptr (fetch-offset r2 o vm))])
       (set-flags a1 a2 vm))]

    [(Cmp (? register? r1) (? integer? v))
     (let ([a1 (reg->val r1 vm)]
           [a2 v])
       (set-flags a1 a2 vm))]

    [(Cmp (Offset r1 o) (? register? r2))
     (let ([a1 (deref-ptr (fetch-offset r1 o vm))]
           [a2 (reg->val r2 vm)])
       (set-flags a1 a2 vm))]

    [(Cmp (Offset r1 o1) (Offset r2 o2))
     (let ([a1 (deref-ptr (fetch-offset r1 o1 vm))]
           [a2 (deref-ptr (fetch-offset r2 o2 vm))])
       (set-flags a1 a2 vm))]

    [(Cmp (Offset r o) (? integer? v))
     (let ([a1 (deref-ptr (fetch-offset r o vm))]
           [a2 v])
       (set-flags a1 a2 vm))])

  (set-vm-ip! vm (add1 (vm-ip vm))))

(define (set-flags a1 a2 vm)
  (cond [(< a1 a2)
         (set-vm-flags! vm (hash 'eq #f 'lt #t 'gt #f))]
        [(= a1 a2)
         (set-vm-flags! vm (hash 'eq #t 'lt #f 'gt #f))]
        [else
         (set-vm-flags! vm (hash 'eq #f 'lt #f 'gt #t))]))


(define (jmp op vm)
  (set-vm-ip! vm (hash-ref (vm-label-map vm) (Jmp-x op))))

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
      (+ (reg->val dst vm) (operand->value (Add-src op) vm))))

    (set-vm-ip! vm (add1 (vm-ip vm)))))


(define (asm-sub op vm)
 (let ([regs (vm-regs vm)]
        [dst (Add-dst op)])
    (set-vm-regs!
     vm
     (hash-set
      regs dst
      (- (reg->val dst vm) (operand->value (Add-src op) vm))))

    (set-vm-ip! vm (add1 (vm-ip vm)))))

;; load-to-mem: Int x Ptr x VM -> VM
;; load-to-mem takes an integer, a pointer and a vm and returns a
;; vm with updated state
(define (load-to-mem v p vm)
  (match (ptr-type p)
    ['s (set-vm-s! vm (list-set (vm-s vm) (ptr-addr p) v))]
    ['h (set-vm-h! vm (list-set (vm-h vm) (ptr-addr p) v))]))

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
