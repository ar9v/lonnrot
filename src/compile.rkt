#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Registers
;; `rdi` is one of the caller-saved registers, and
;; it will have to be used for, e.g., write-byte
(define rax 'rax) ;; Return value
(define rbx 'rbx) ;; Heap
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
  (match p
    [(Prog defs expr)
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
      (compile-defines defs))]))

;; compile-define: Defn
;; To compile a function definition we have to
;; - Provide an ASM conformant label
;; - Compile the body with an environment made up of the
;;   parameters
;;
;; The parameters are reversed so that the rightmost param,
;; the latest, is at the top of the stack
(define (compile-define d)
  (match d
    [(Defn f params body)
     (seq (Label (symbol->label f))
          (compile-e body (parity (cons #f (reverse params))))
          (Ret))]))

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

(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))


;; compile-e: AST x CEnv -> Asm (x86 AST)
;; (compile-e e c) takes an expression and a compile-time environment
;; and produces the appropriate Asm (a86) AST representation
;;
;; a CEnv is a list of variables, where the lookup consists of finding
;; the index of a given variable (this is known as lexical addressing)
(define (compile-e expr cenv)
  (match expr
    ;; Atomic data
    [(Int i)           (compile-value i)]
    [(Bool b)          (compile-value b)]
    [(Char c)          (compile-value c)]
    [(Eof)             (compile-value eof)]
    [(Empty)           (compile-value '())]
    [(Var x)           (compile-var x cenv)]

    ;; Primitives
    [(Prim0 p)         (compile-prim0 p cenv)]
    [(Prim1 p expr)    (compile-prim1 p expr cenv)]
    [(Prim2 p e1 e2)   (compile-prim2 p e1 e2 cenv)]

    ;; Control, Sequencing, Variable bindings
    [(If e1 e2 e3)     (compile-if e1 e2 e3 cenv)]
    [(Begin e1 e2)     (compile-begin e1 e2 cenv)]
    [(Let x e1 e2)     (compile-let x e1 e2 cenv)]

    ;; Function Application
    [(App f es)        (compile-app f es cenv)]))


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
     (seq (assert-integer rax cenv)
          (Add rax (immediate->bits 1)))]

    ['sub1
     (seq (assert-integer rax cenv)
          (Sub rax (immediate->bits 1)))]

    ;; Predicates
    ['zero?
     (let ([l1 (gensym)])
       (seq (assert-integer rax cenv)
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
     (seq (assert-char rax cenv)
          (Sar rax char-shift)
          (Sal rax int-shift))]

    ['integer->char
     (seq (assert-codepoint cenv)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]

    ;; IO
    ['write-byte
     (seq (assert-byte cenv)
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
     (seq (assert-cons rax cenv)
          (Xor rax type-cons)
          (Mov rax (Offset rax 8)))]

    ['cdr
     (seq (assert-cons rax cenv)
          (Xor rax type-cons)
          (Mov rax (Offset rax 0)))]))


(define (compile-p2 p cenv)
  (match p
    ;; When we have reached this point, our primitive's
    ;; first operand is in the stack, and our second operand
    ;; has been compiled with the #f-extended environment
    ['+ (seq (Pop r8)
             (assert-integer r8 cenv)  ;; Check operand 1 is an integer
             (assert-integer rax cenv) ;; Check operand 2 is an integer
             (Add rax r8))]

    ;; The procedure is very similar. However, since r8 is our
    ;; /first/ operand and subtraction is NOT commutative, we must
    ;; subtract rax from r8, which leaves the result in r8, hence
    ;; the last Mov instruction
    ['- (seq (Pop r8)
             (assert-integer r8 cenv)
             (assert-integer rax cenv)
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

;; Sequence
(define (compile-begin e1 e2 cenv)
  (seq (compile-e e1 cenv)
       (compile-e e2 cenv)))

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

(define (compile-app f args cenv)
  (if (even? (+ (length args) (length cenv)))

      ;; If the stack is aligned, generate code for the arguments
      ;; and  simply call the function
      ;; After calling the function, pop off the arguments from
      ;; the stack
      (seq (compile-es args cenv)
           (Call (symbol->label f))
           (Add rsp (* 8 (length args))))

      ;; If it is not aligned, adjust the stack by growing the stack
      ;; Then compile the expressions with the padded environment
      ;; Lastly, call the function and popoff the arguments and the
      ;; extra space that was padded
      (seq (Sub rsp 8)
           (compile-es args cenv)
           (Call (symbol->label f))
           (Add rsp (* 8 (add1 (length args)))))))

;; TODO
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
  (lambda (arg cenv)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne (error-label cenv)))))

(define assert-integer
  (assert-type mask-int type-int))

(define assert-char
  (assert-type mask-char type-char))

(define assert-box
  (assert-type ptr-mask type-box))

(define assert-cons
  (assert-type ptr-mask type-cons))

;; This is basically the comparison
;; we made in the interpreter
(define (assert-codepoint cenv)
  (let ([ok (gensym)])
    (seq (assert-integer rax cenv)
         (Cmp rax (immediate->bits 0))
         (Jl (error-label cenv))
         (Cmp rax (immediate->bits 1114111))
         (Jg (error-label cenv))
         (Cmp rax (immediate->bits 55295))
         (Jl ok)
         (Cmp rax (immediate->bits 57344))
         (Jg ok)
         (Jmp (error-label cenv))
         (Label ok))))

(define (assert-byte cenv)
  (seq (assert-integer rax cenv)
       (Cmp rax (immediate->bits 0))
       (Jl (error-label cenv))
       (Cmp rax (immediate->bits 255))
       (Jg (error-label cenv))))

;; Error labeling
;; error-label: Cenv -> Label
;;
;; Since we now have more than one type of error
;; we abstract the error checking
;;
;; Basically, if our stack is aligned, it means
;; that the error is your normal 'err type
(define (error-label cenv) 'raise_error)
