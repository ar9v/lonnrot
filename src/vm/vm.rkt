#lang racket
(provide run)
(require
 a86 racket/struct "types.rkt")
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
  (ip sp regs code s h labelMap halt)
  #:mutable

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'vm)
      (lambda (obj) (list (vm-ip obj)
                          (vm-sp obj)
                          (vm-regs obj)
                          (vm-code obj)
                          (vm-s obj)
                          (vm-h obj)
                          (vm-labelMap obj)
                          (vm-halt obj)))))])

(define test-file "test.obj")

;; getHalt: [#(Op _ ...)] x Int -> Int
;; getHalt takes a list of ops and an integer which represents
;; an accumulator; this will tell us where is the return for
;; 'entry
(define (getHalt code ip)
  (match (car code)
    [(Ret) ip]
    [_ (getHalt (cdr code) (add1 ip))]))

(define (getAddrLabels code addrHash ip)
  (match code
    ['() addrHash]
    [_
     (match (car code)
       [(Label l) (hash-set addrHash l ip)]
       [_ (getAddrLabels (cdr code) addrHash (add1 ip))])]))

;; main: String -> Answer
;; Main reads an image file, which is a list of a86 ops
;; and calls the VM's run function
(define (main image-file)
  (let* ([filename-port (open-input-file image-file)]
         [code (eval (read filename-port))]
         [labelMap (getAddrLabels code (hash) 0)]
         [halt (getHalt code 0)])
    (run
     (vm
      0  ;; ip
      1  ;; stack pointer
      (hash 'rax 0 'rbx 0 'rcx 0 'rdx 0 'r8  0 'r9  0 'rdi 0) ;; registers
      code ;; [Instruction]
      '()  ;; Stack
      '()  ;; Heap TODO: this must be initialized with `heap_size` (1000) values, since this doesn't "grow" like the stack
           ;; TODO: add a heap pointer? we must also keep track of the current position... although this could simply be rbx
      labelMap ;; <Symbol, Integer (Address)>
      halt)))) ;; Int

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

;; op-dispatch: VM -> VM
;; op-dispatch takes a vm (i.e. a state in the running code) and depending
;; on the operation pointed to by ip, modifies the vm's state and returns
;; the new state
(define (op-dispatch vm)
  (let ([op (load-op vm)])
    (match op
      [(Extern _)
       (set-vm-ip! vm (add1 (vm-ip vm))) vm]
      [(Label l)
       (set-vm-ip! vm (add1 (vm-ip vm))) vm]
      [(Mov _ _)
       (mov op vm)]
      [(Ret)
       (set-vm-ip! vm (add1 (vm-ip vm))) vm])))

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
       (load-to-mem (reg->val src) (fetch-offset dst o) vm)]

      ;; Dst = Offset, Src = Integer
      [(Mov (Offset dst o) (? integer? v))
       (load-to-mem v (fetch-offset dst o))]

      [_ (error "Invalid MOV operation" op)])

    (set-vm-ip! vm (add1 (vm-ip vm)))))


;; load-to-mem: Int x Ptr x VM -> VM
;; load-to-mem takes an integer, a pointer and a vm and returns a
;; vm with updated state
(define (load-to-mem v p vm)
  (match (ptr-type p)
    ['s (set-vm-s! vm (list-set (vm-s vm) (ptr-addr p) v))]
    ['h (set-vm-h! vm (list-set (vm-h vm) (ptr-addr p) v))]))

(define (reg->val reg vm)
  (hash-ref
   (vm-regs vm)
   reg))

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
