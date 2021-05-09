#lang racket
(provide compile)
(require "ast.rkt" "types.rkt" a86/ast)

;; Registers
;; rdi is one of the caller-saved registers, and
;; it will have to be used for, e.g., write-byte
(define rax 'rax)
(define rbx 'rbx)
(define r8  'r8)
(define rsp 'rsp)
(define rdi 'rdi)


;; compile: Expr -> Asm (x86 AST)
;; compile takes an expression represented in our
;; AST (the result from parsing) and emits an x86
;; AST representation
;;
;; This function wraps the actual source compilation
;; between the "boilerplate" that precedes the entry
;; label in the `.s` file
(define (compile expr)
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
   (compile-e expr '())
   (Ret)

   ;; Now, we can either have an error because the stack
   ;; is unaligned or because of some other type of
   ;; error (e.g. type mismatch) denoted as 'err.
   ;;
   ;; So, an alignment error simply aligns the stack and
   ;; throws 'err
   (Label 'raise_error_align)
   (Sub rsp 8)
   (Jmp 'raise_error)))

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
    [(Var x)           (compile-var x cenv)]

    ;; Primitives
    [(Prim0 p)         (compile-prim0 p cenv)]
    [(Prim1 p expr)    (compile-prim1 p expr cenv)]
    [(Prim2 p e1 e2)   (compile-prim2 p e1 e2 cenv)]

    ;; Control, Sequencing, Variable bindings
    [(If e1 e2 e3)     (compile-if e1 e2 e3 cenv)]
    [(Begin e1 e2)     (compile-begin e1 e2 cenv)]
    [(Let x e1 e2)     (compile-let x e1 e2 cenv)]))


;; Expressions
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

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
          (Add rax (value->bits 1)))]

    ['sub1
     (seq (assert-integer rax cenv)
          (Sub rax (value->bits 1)))]

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

    ['eof-object?
     (let ([l1 (gensym)])
       (seq (Cmp rax val-eof)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]

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
          (Mov rax val-void))]))

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
             (Mov rax r8))]))

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

;; Assertions
(define (assert-type mask type)
  (lambda (arg cenv)
    (seq (Mov rbx arg)
         (And rbx mask)
         (Cmp rbx type)
         (Jne (error-label cenv)))))

(define assert-integer
  (assert-type mask-int type-int))

(define assert-char
  (assert-type mask-char type-char))

;; This is basically the comparison
;; we made in the interpreter
(define (assert-codepoint cenv)
  (let ([ok (gensym)])
    (seq (assert-integer rax cenv)
         (Cmp rax (value->bits 0))
         (Jl (error-label cenv))
         (Cmp rax (value->bits 1114111))
         (Jg (error-label cenv))
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp (error-label cenv))
         (Label ok))))

(define (assert-byte cenv)
  (seq (assert-integer rax cenv)
       (Cmp rax (value->bits 0))
       (Jl (error-label cenv))
       (Cmp rax (value->bits 255))
       (Jg (error-label cenv))))


;; Error labeling
;; error-label: Cenv -> Label
;;
;; Since we now have more than one type of error
;; we abstract the error checking
;;
;; Basically, if our stack is aligned, it means
;; that the error is your normal 'err type
(define (error-label cenv)
  (if (even? (length cenv))
      'raise_error
      'raise_error_align))
