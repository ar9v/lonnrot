#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Registers
;; `rdi` is one of the caller-saved registers, and
;; it will have to be used for, e.g., write-byte
(define rax 'rax) ;; Return value
(define rbx 'rbx) ;; Heap
(define rcx 'rcx) ;; scratch
(define rdx 'rdx) ;; Second return register
(define r8  'r8)  ;; Placeholder in + and -
(define r9  'r9)  ;; Placeholder in assert-type
(define rsp 'rsp) ;; Stack Pointer
(define rdi 'rdi) ;; Argument register (runtime calls)


;; compile: Expr -> ASM (x86 AST)
;; Wraps the compilation of a program within a Prog AST node
;;
;; compile takes an expression represented in our
;; AST (the result from parsing) and emits an x86
;; AST representation
;;
;; This function wraps the actual source compilation
;; between the "boilerplate" that precedes the entry
;; label in the `.s` file
(define (compile p)
  (match (label-lambdas (desugar p))
    ;; UPDATE (loot): Since we desugar, there will be
    ;; no definitions
    [(Prog '() expr)
     (prog
      ;; These are the C defined functions in the runtime
      (Extern 'peek_byte)
      (Extern 'read_byte)
      (Extern 'write_byte)
      (Extern 'raise_error)

      ;; Before calling functions from other object files (e.g.
      ;; the IO functions in io.c), the stack has to be
      ;; aligned to 16 bytes (according to Sys V ABI convention)
      ;; So we decrement a word from the stack pointer and then
      ;; restore it.
      ;;
      ;; UPDATE (Fraud): however, since we will be pushing lexical
      ;; addresses onto the stack (due to `let`), we will have to
      ;; check per-case if we need to align the stack. Hence, we
      ;; delete our (Sub rsp 8) line.
      (Label 'entry)
      ;; UPDATE (Hustle): We pass the heap pointer to rbx
      (Mov rbx rdi)
      ;; UPDATE (Iniquity): we pad the environment
      (compile-e expr '(#f))
      (Mov rdx rbx) ; return the heap pointer in second return register
      (Ret)
      ;; Since we desugar, we have no defines. Instead, we have
      ;; scattered lambdas. We retrieve them and compile them
      (compile-lambda-definitions (get-lambdas expr)))]))

;; To compile a function definition we have to
;; - Provide an ASM conformant label
;; - Compile the body with an environment made up of the
;;   parameters
;;
;; The parameters are reversed so that the rightmost param,
;; the latest, is at the top of the stack
;;
;; (cons #f (reverse params))
;; That #f represents the space that is used when Call pushes the
;; return pointer onto the stack.
;; UPDATE (jig):
;; We want to compile this as a tail call. To this effect we propagate
;; the amount of params we have, as this is quantity is what we'll have to
;; "skip" when moving values around in memory
(define (compile-lambda-definition l)
  (match l
    [(Lambda '() _ _) (error "Lambdas must be labeled to be compiled")]
    [(Lambda name params body)
     (let* ((free-vars (remq* params (free-variables body)))
            (env (parity (cons #f (cons #f (reverse (append params free-vars)))))))

       (seq (Label (symbol->label name))
            (compile-e body env)
            (Ret)))]))

(define (compile-lambda-definitions ls)
  ;; JMCT wraps everything in seq but is it really necessary?
  (match ls
    ['() (seq)]
    [(cons d ds)
     (seq (compile-lambda-definition d)
          (compile-lambda-definitions ds))]))

;; We append (#f) rather than cons it so we
;; because we want to align the stack, but not shift
;; the stuff that's on top of it
;;
;; The precondition for issuing a `Call` is to have
;; 16-byte alignment in the stack. But Call itself pushes the
;; return address to the stack, so we must generate code that
;; makes it so that the stack is not aligned
(define (parity cenv)
  (if (even? (length cenv))
      (append cenv (list #f))
      cenv))

;; compile-tail-e: AST Expr x CEnv x Int -> Asm (x86)
;; compile-tail-e takes an expression represented as an AST node,
;; a Compile Time Environment and the Amount of things that need
;; to be skipped in memory, and generates code that performs a
;; tail call
;;
;; From all possible nodes of the AST, only those with subexpressions
;; are in need of specific tail call implementations, i.e. a value such
;; as an integer is itself "the last thing to be executed" in itself.
(define (compile-tail-e expr cenv size)
  (match expr
    [(If p c a)     (compile-tail-if p c a cenv size)]
    [(Begin e1 e2)  (compile-tail-begin e1 e2 cenv size)]
    [(Let x e1 e2)  (compile-tail-let x e1 e2 cenv size)]
    [(LetRec bindings body)
     (compile-tail-letrec (map car bindings) (map cadr bindings) body cenv)]
    [(App f args)
     (if (<= (length args) size)
         (compile-tail-call f args cenv)
         (compile-call f args cenv))]

    [_              (compile-e expr cenv)]))

;; compile-e: AST x CEnv -> Asm (x86 AST)
;; (compile-e e c) takes an expression and a compile-time environment
;; and produces the appropriate Asm (a86) AST representation
;;
;; a CEnv is a list of variables, where the lookup consists of finding
;; the index of a given variable (this is known as lexical addressing)
(define (compile-e expr cenv)
  (match expr
    ;; Atomic data
    [(Int i)                    (compile-value i)]
    [(Bool b)                   (compile-value b)]
    [(Char c)                   (compile-value c)]
    [(Eof)                      (compile-value eof)]
    [(Empty)                    (compile-value '())]
    [(Var x)                    (compile-var x cenv)]
    [(Lambda name params body)  (compile-lambda name params (free-variables expr) cenv)]

    ;; Primitives
    [(Prim0 p)         (compile-prim0 p cenv)]
    [(Prim1 p expr)    (compile-prim1 p expr cenv)]
    [(Prim2 p e1 e2)   (compile-prim2 p e1 e2 cenv)]

    ;; Control, Sequencing, Variable bindings
    [(If e1 e2 e3)     (compile-if e1 e2 e3 cenv)]
    [(Begin e1 e2)     (compile-begin e1 e2 cenv)]
    [(Let x e1 e2)     (compile-let x e1 e2 cenv)]
    [(LetRec bindings body)

     (compile-letrec (map car bindings)
                     (map cadr bindings)
                     body
                     cenv)]

    ;; Function Application
    [(App f args) (compile-call f args cenv)]))


;; Expressions
(define (compile-value v)
  (seq (Mov rax (immediate->bits v))))

;; Looking up a var is checking how many steps
;; into the stack it is found. We can't know the value
;; at compile time, but we can now /where the value will be/
(define (compile-var x cenv)
  (let ([i (lookup x cenv)])
    (seq (Mov rax (Offset rsp i)))))

(define (compile-prim0 p cenv)
  (match p
    ['void      (seq (Mov rax val-void))]

    ['read-byte (seq (pad-stack cenv)
                     (Call 'read_byte)
                     (unpad-stack cenv))]

    ['peek-byte (seq (pad-stack cenv)
                     (Call 'peek_byte)
                     (unpad-stack cenv))]))

;; Since we have to check for errors, we'll have
;; some helper functions per primitive

;; Now, compile-prim1 requires logic for
;; compiling the primitive and the expression. The idea is that
;; with errors we can't indiscriminately generate code for
;; each primitive.
(define (compile-prim1 p expr cenv)
  (seq (compile-e expr cenv)
       (compile-p1 p cenv)))

(define (compile-prim2 p e1 e2 cenv)
  (seq (compile-e e1 cenv)
       (Push rax)
       (compile-e e2 (cons #f cenv))
       (compile-p2 p cenv)))

;; compile-p1: Symbol (Primitive) x CEnv -> ASM
;; Since we compiled the primitive's expression in
;; `compile-prim1` we only need the primitive and the
;; environment
(define (compile-p1 p cenv)
  (match p
    ;; Increment/Decrement
    ['add1
     (seq (assert-integer rax)
          (Add rax (immediate->bits 1)))]

    ['sub1
     (seq (assert-integer rax)
          (Sub rax (immediate->bits 1)))]

    ;; Predicates
    ['zero?
     (let ([l1 (gensym)])
       (seq (assert-integer rax)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]

    ['char?
     (let ([l1 (gensym)])
       ;; AND gives us the last two bits
       ;; XOR will produce 0 if the last two bits are
       ;; the same, i.e. if the last bits are 01, type char
       (seq (And rax mask-char)
            (Xor rax type-char)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]

    ['eof-object? (eq-imm val-eof)]
    ['empty? (eq-imm val-empty)]

    ;; Value conversions
    ;; To convert between values, the idea is simple:
    ;; we strip their respective type tags, and then
    ;; add the target type's type tag
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]

    ['integer->char
     (seq assert-codepoint
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]

    ;; IO
    ['write-byte
     (seq assert-byte
          (pad-stack cenv)
          (Mov rdi rax)
          (Call 'write_byte)
          (unpad-stack cenv)
          (Mov rax val-void))]

    ;; Inductive Data
    ['box
     (seq (Mov (Offset rbx 0) rax)
          (Mov rax rbx)
          (Or rax type-box)
          (Add rbx 8))]

    ['unbox
     (seq (assert-box rax)
          (Xor rax type-box)
          (Mov rax (Offset rax 0)))]

    ['car
     (seq (assert-cons rax)
          (Xor rax type-cons)
          (Mov rax (Offset rax 8)))]

    ['cdr
     (seq (assert-cons rax)
          (Xor rax type-cons)
          (Mov rax (Offset rax 0)))]))


(define (compile-p2 p cenv)
  (match p
    ;; When we have reached this point, our primitive's
    ;; first operand is in the stack, and our second operand
    ;; has been compiled with the #f-extended environment
    ['+ (seq (Pop r8)
             (assert-integer r8)  ;; Check operand 1 is an integer
             (assert-integer rax) ;; Check operand 2 is an integer
             (Add rax r8))]

    ;; The procedure is very similar. However, since r8 is our
    ;; /first/ operand and subtraction is NOT commutative, we must
    ;; subtract rax from r8, which leaves the result in r8, hence
    ;; the last Mov instruction
    ['- (seq (Pop r8)
             (assert-integer r8)
             (assert-integer rax)
             (Sub r8 rax)
             (Mov rax r8))]

    ['eq?
     (let ([l1 (gensym)])
       (seq (Cmp rax (Offset rsp 0))
            (Sub rsp 8)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]

    ['cons
     (seq (Mov (Offset rbx 0) rax)
          (Pop rax)
          (Mov (Offset rbx 8) rax)
          (Mov rax rbx)
          (Or rax type-cons)
          (Add rbx 16))]))

;; Conditional
(define (compile-if e1 e2 e3 cenv)
  (let ([l1 (gensym 'if)]
        [l2 (gensym 'if)])
    (seq (compile-e e1 cenv)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 cenv)
         (Jmp l2)
         (Label l1)
         (compile-e e3 cenv)
         (Label l2))))

(define (compile-tail-if e1 e2 e3 cenv size)
  (let ([l1 (gensym 'if)]
        [l2 (gensym 'if)])
    (seq (compile-e e1 cenv)
         (Cmp rax val-false)
         (Je l1)
         (compile-tail-e e2 cenv size)
         (Jmp l2)
         (Label l1)
         (compile-tail-e e3 cenv size)
         (Label l2))))

;; Sequence
(define (compile-begin e1 e2 cenv)
  (seq (compile-e e1 cenv)
       (compile-e e2 cenv)))

(define (compile-tail-begin e1 e2 cenv size)
  (seq (compile-e e1 cenv)
       (compile-tail-e e2 cenv size)))

;; Variable bindings (let)
;; To bind a variable is to
;; - Compile it in the current environment
;; - Push it (its symbol) onto the stack
;; - Compile the body with the environment
;;   extended by the binding
;; - And finally, adding a byte to the stack pointer
;;   i.e. the binding goes out of scope
(define (compile-let x e1 e2 cenv)
  (seq (compile-e e1 cenv)
       (Push rax)
       (compile-e e2 (cons x cenv))
       (Add rsp 8)))

(define (compile-tail-let x e1 e2 cenv size)
  (seq (compile-e e1 cenv)
       (Push rax)
       (compile-tail-e e2 (cons x cenv) size)
       (Add rsp 8)))

;; Letrec
;; compile-letrec: [Vars] x [Lambda] x Expr x CEnv -> ASM
;; compile-letrec takes a list of symbols to be bound,
;; a list of functions which will be the values of the vars
;; a body, and an environment and generates the code to perform
;; a letrec
(define (compile-letrec lhs fs body cenv)
  (seq
   ;; First we generate the closures /without initializing them/
   ;; This binds the variables to the prospective closures
   (compile-letrec-lambdas fs cenv)

   ;; Then, since we have already bound all variables to the prospective
   ;; closures, we can safely initialize the closures with the complete
   ;; context...
   (compile-letrec-init lhs fs (append (reverse lhs) cenv))

   ;; ... which lets us compile the body in the freshly extended env
   (compile-e body (append (reverse lhs) cenv))

   ;; And we deallocate the space
   (Add rsp (* 8 (length fs)))))

(define (compile-tail-letrec lhs fs body cenv)
  (seq
   (compile-letrec-lambdas fs cenv)
   (compile-letrec-init lhs fs (append (reverse lhs) cenv))
   (compile-tail-e body (append (reverse lhs) cenv))
   (Add rsp (* (length fs)))))

;; compile-letrec-lambdas: [Lambda] x CEnv -> ASM
;; compile-letrec-lambdas pushes uninitialized closures
;; of each f in fs onto the stack
;;
;; One could be tempted to simply call compile-lambda, but
;; that would initialize the closure, hence a bit of
;; repetition
(define (compile-letrec-lambdas ls cenv)
  (match ls
    ['() (seq)]
    [(cons f fs)
     (match f
       [(Lambda name params body)
        (let ([frees (free-variables f)])
          (seq
           (Lea rax (symbol->label name))
           (Mov (Offset rbx 0) rax)
           (Mov rax (length frees))
           (Mov (Offset rbx 8) rax)
           (Mov rax rbx)
           (Or rax type-proc)
           (Add rbx (* 8 (+ 2 (length frees))))
           (Push rax)
           (compile-letrec-lambdas fs (cons #f cenv))))])]))

;; compile-letrec-init: [Symbol] x [Lambda] x CEnv -> ASM
;; compile-letrec-init initializes the values in the closure.
;; This has to be done because we want to associate the closures to the
;; variables first. This way, if we reference another function in the
;; body of the nth function, we have the value of the pointer in the
;; extended environment
(define (compile-letrec-init vars ls cenv)
  (match vars
    ['() (seq)]
    [(cons v vs)
     ;; (first ls) because that's the lambda that corresponds
     ;; to the current variable, v
     (let ([frees (free-variables (first ls))])

       (seq
        ;; Move the closure's reference to r9. Remember, by now
        ;; the variable v is bound to an unitialized closure; this
        ;; returns the function pointer to r9
        (Mov r9 (Offset rsp (lookup v cenv)))
        ;; Strip off the tag so we can use the address
        (Xor r9 type-proc)

        (Add r9 16) ;; skip to the env
        (copy-env-to-heap frees cenv 0)
        (compile-letrec-init vs (rest ls) cenv)))]))

;; Lambdas
;;
;; compile-lambda: Symbol x [Vars] x Cenv -> ASM
;; compile-lambda produces the instructions to compile a
;; closure. A closure is simply a context, a structure to hold
;; a lambda's information
(define (compile-lambda name params frees cenv)
  (seq
   ;; Load the function's address to rax
   ;; and move it to the heap
   (Lea rax (symbol->label name))
   (Mov (Offset rbx 0) rax)

   ;; Save the environment
   ;; For this we must know how much memory we need, and
   ;; we then make space for it in the heap
   ;;
   ;; Move the amount of free vars to the next address
   (Mov r8 (length frees))
   (Mov (Offset rbx 8) r8)

   ;; Move the heap's address to r9 and skip the label and
   ;; the length, and copy the environment to the heap
   (Mov r9 rbx)
   (Add r9 16)
   (copy-env-to-heap frees cenv 0)

   ;; Return a pointer to the closure:
   ;; Move the address to rax and tag it, and allocate
   ;; the space, which is the amount of stuff + the length + label
   (Mov rax rbx)
   (Or rax type-proc)
   (Add rbx (* 8 (+ 2 (length frees))))))

;; copy-env-to-heap: [Vars] x CEnv x Int -> ASM
;; Generates code that actually moves the env values onto the
;; heap.
(define (copy-env-to-heap frees cenv offset)
  (match frees
    ['() (seq)]
    [(cons v vars)
     (seq
      ;; Lookup the variable
      (Mov r8 (Offset rsp (lookup v cenv)))

      ;; Put it in the heap
      ;; We use r9 because that's where the
      ;; environment starts. If we use rbx, we'll clobber
      ;; the label and length(!)
      (Mov (Offset r9 offset) r8)
      (copy-env-to-heap vars cenv (+ 8 offset)))]))

;; compile-call: Expr x [Vars] x CEnv -> ASM
;; compile-call takes an expression, asserts it's a function pointer
;; and then moves its args to the stack, passing along the environment
;; captured in the closure
(define (compile-call f args cenv)
  (let* ([num-args (length args)]
         [aligned (even? (+ num-args (length cenv)))]
         [i (if aligned 1 2)]
         [env+ (if aligned
                   cenv
                   (cons #f cenv))]
         [env++ (cons #f env+)]) ;; See compile-lambda-definitions

    (seq
     ;; Align the stack if necessary
     (if aligned
         (seq)
         (Sub rsp 8))

     ;; Generate the code for f, which puts its result in rax
     ;; so we can push that onto the stack
     ;;
     ;; Since f is (allegedly) a lambda, this means not only that we
     ;; get the pointer back at rax, but also that we have pushed the
     ;; amount of free variables onto the heap, see compile-lambda
     (compile-e f env+)
     (Push rax)

     ;; Then, we generate the code for evaluating the args
     ;; We use env++ because we have the extra #f padding
     ;; (which stands for the function expression we just generated)
     ;; compile-es pushes args onto the stack
     (compile-es args env++)

     ;; Now, we fetch what is supposed to be the function
     ;; pointer from the stack, we assert it is and remove the tag
     (Mov rax (Offset rsp (* 8 num-args)))
     (assert-proc rax)
     (Xor rax type-proc)

     ;; The function pointer points to a closure, so we must
     ;; copy the closure environment onto the stack
     (copy-closure-env-to-stack)

     ;; We move the size of the environment onto the stack
     ;; Again, the next thing in memory from the address is the amount
     ;; of free variables
     (Mov rcx (Offset rax 8))
     (Push rcx)

     (Call (Offset rax 0))

     ;; Get the size of the environment off the stack
     (Pop rcx)
     (Sal rcx 3)

     ;; Pop the arguments
     (Add rsp (* 8 (+ i num-args))) ;; Accounts for args and padding
     (Add rsp rcx)))) ;; Accounts for the closure

;; compile-tail-call: Expr x [Exprs] x CEnv -> ASM
(define (compile-tail-call f args cenv)
  (let ([num-args (length args)])
    (seq
     (compile-e f cenv)
     (Push rax)
     (compile-es args (cons #f cenv))

     ;; Now move them to the stack spaces that we can clobber
     ;; These are at rsp offsets whose distances are
     ;; the amount of things put on the stack by compile-es plus
     ;; all the other thing that might've been in context (e.g. if
     ;; we call a function inside a `let`)
     ;;
     ;;  0 | arg1 | arg2 | frame-var 1 | ... | #f (return pointer) | free real estate | ...
     ;;  UPDATE (loot): we now acnum-args for the function pointer and the length
     (move-args num-args (+ num-args (+ 2 (in-frame cenv))))

     ;; Retrieve the function pointer, assert and untag
     (Mov rax (Offset rsp (* 8 num-args)))
     (assert-proc rax)
     (Xor rax type-proc)

     ;; We can finally shave off the upper part of the stack up until
     ;; the return pointer, since we reused the old frame!
     ;; Again, we account for the function pointer and the length
     (Add rsp (* 8 (+ num-args (+ 2 (in-frame cenv)))))

     (copy-closure-env-to-stack)

     (Jmp (Offset rax 0)))))

;; copy-closure-env-to-stack -> ASM
;; copy-closure-env-to-stack generates the instructions needed to copy
;; a closure provided that we fulfill the invariant of having a
;; function pointer at rax
(define (copy-closure-env-to-stack)
  (let ([loop-label (symbol->label (gensym 'copy_closure))]
        [done-label (symbol->label (gensym 'copy_done))])

    (seq

     ;; Get the length of the closure
     (Mov r8 (Offset rax 8))

     (Mov r9 rax)
     ;; This is where the closure's env starts
     ;; Again, we skip over the function pointer and the length
     (Add r9 16)

     ;; We start the copy
     (Label loop-label)
     ;; Are there no more things left to copy?
     (Cmp r8 0)
     (Je done-label) ;; if so, jump to the end

     ;; If not:
     ;; - Move whatever is pointed to by r9 to the stack
     ;; - Increment r9
     ;; - decrement r8
     ;; - Jump back to the loop-label
     (Mov rcx (Offset r9 0))
     (Push rcx)
     (Add r9 8)
     (Sub r8 1)
     (Jmp loop-label)

     ;; We are done
     (Label done-label))))

;; move-args: Int x Int -> ASM
;; move-args checks how many args are left to move and how many spaces (i.e. the offset)
;; they have to be moved from rsp
(define (move-args num-items num-spaces)
  (if (= num-items 0)
      (seq) ;; You are done, nothing else to move

      (seq
       ;; (* 8 (sub1 num-items)) is the offset to the first argument of the
       ;; function call, we store that in a scratch register...
       (Mov r9 (Offset rsp (* 8 (sub1 num-items))))

       ;; ...and then move it to the slot that is in the "original frame"
       ;; (i.e. after the return pointer). This is the amount of stuff
       ;; in the current frame plus how many items you have to move
       (Mov (Offset rsp (* 8 (+ num-items num-spaces))) r9)

       ;; And we call, but with one less item, gradually coming closer to
       ;; the top of the stack
       (move-args (sub1 num-items) num-spaces))))

;; in-frame: CEnv -> Int
;; in-frame gives us the amount of stuff that stands between the
;; top of the stack (where rsp points) and the return pointer
;; produced by a Call
(define (in-frame cenv)
  (match cenv
    ['() 0]
    [(cons #f rest) 0] ;; We found the return pointer
    [(cons y rest) (+ 1 (in-frame rest))]))

;; compile-es: [Exprs] x Cenv -> (Seq ...)
;; compile-es provides a wrapper to sequence the code generated
;; from compiling a list of expressions, usually the args provided to a function
(define (compile-es exprs cenv)
  (match exprs
    ['() '()]
    [(cons e es)
     (seq (compile-e e cenv)
          (Push rax)
          (compile-es es (cons #f cenv)))]))

;; Lookup
;; Lookup iterates over the environment and
;; returns the index of the variable it finds.
;; Notice:
;; - We increment by 8, i.e. byte by byte
;; - This lookup gives us the /most recent/ binding of
;;   `v`. This shadows variables (!!)
(define (lookup v cenv)
  (match cenv
    ['() (error "Undefined variable: " v)]
    [(cons y rest)
     (if (eq? v y)
         0
         (+ 8 (lookup v rest)))]))


;; Stack padding and unpadding
;; Since we now may or may not make calls, we
;; align the stack on a per-call basis. Likewise,
;; we undo the stack alignment after each call
(define (pad-stack cenv)
  (if (even? (length cenv))
      (seq (Sub rsp 8))
      (seq)))


(define (unpad-stack cenv)
  (if (even? (length cenv))
      (seq (Add rsp 8))
      (seq)))

;; eq-imm: Immediate -> AST Asm
;; Little helper for compiling value predicates
;; (e.g. val-empty and val-eof)
(define (eq-imm imm)
  (let ([l1 (gensym)])
    (seq (Cmp rax imm)
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))

;; symbol->label: Symbol -> ASM Label
;; Takes a Racket symbol and checks if it has illegal characters
;; to produce a valid ASM label
(define (symbol->label s)
    (string->symbol
     (string-append
      "label_"
      (list->string
       (map (λ (c)
              (if (or (char<=? #\a c #\z)
                      (char<=? #\A c #\Z)
                      (char<=? #\0 c #\9)
                      (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                  c
                  #\_))
           (string->list (symbol->string s))))
      "_"
      (number->string (eq-hash-code s) 16))))

;; Assertions
(define (assert-type mask type)
  (lambda (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'raise_error))))

(define assert-integer
  (assert-type mask-int type-int))

(define assert-char
  (assert-type mask-char type-char))

(define assert-box
  (assert-type ptr-mask type-box))

(define assert-cons
  (assert-type ptr-mask type-cons))

(define assert-proc
  (assert-type ptr-mask type-proc))

;; This is basically the comparison
;; we made in the interpreter
(define (assert-codepoint cenv)
  (let ([ok (gensym)])
    (seq (assert-integer rax cenv)
         (Cmp rax (immediate->bits 0))
         (Jl 'raise_error)
         (Cmp rax (immediate->bits 1114111))
         (Jg 'raise_error)
         (Cmp rax (immediate->bits 55295))
         (Jl ok)
         (Cmp rax (immediate->bits 57344))
         (Jg ok)
         (Jmp 'raise_error)
         (Label ok))))

(define (assert-byte cenv)
  (seq (assert-integer rax cenv)
       (Cmp rax (immediate->bits 0))
       (Jl 'raise_error)
       (Cmp rax (immediate->bits 255))
       (Jg 'raise_error)))
