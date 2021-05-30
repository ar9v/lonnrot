#lang racket
(provide (all-defined-out))

(struct Prog (defs es)            #:prefab)
(struct Halt ()                   #:prefab)

;; Values
(struct Eof ()                    #:prefab)
(struct Empty ()                  #:prefab)
(struct Int (i)                   #:prefab)
(struct Bool (b)                  #:prefab)
(struct Char (c)                  #:prefab)
(struct String (s)                #:prefab)
(struct Var (x)                   #:prefab)

;; Primitives
(struct Prim0 (p)                 #:prefab)
(struct Prim1 (p e)               #:prefab)
(struct Prim2 (p e1 e2)           #:prefab)

;; Conditional, Sequencing
(struct If (e1 e2 e3)             #:prefab)
(struct Cond (clauses)            #:prefab)
(struct Begin (args)              #:prefab)

;; Variable binding
(struct Let (x e1 e2)             #:prefab)
(struct LetRec (bindings body)    #:prefab)

;; Function Application
(struct Defn (f params body)      #:prefab)

;;;; We can have an arbitrary expression as a function to
;;;; be applied, for example, a lambda
(struct App (expr args)           #:prefab)

;; Lambdas
(struct Lambda (name params body) #:prefab)
(struct Formals (vars)            #:prefab)


;; AST Transformations ;;

;; free-variables: Expr -> [Var]
;; free-variables produces a list of Var's which are free
;; in a given expression's context.
;;
;; A variable is free if it is NOT bound in the immediate
;; context, for example in:
;;    (let ((x 4)) (+ x y))
;; `y` is free
(define (free-variables expr)
  (define (free-variables expr)
    (match expr
      [(Var x) (list x)]
      [(Prim1 p e) (free-variables e)]
      [(Prim2 p e1 e2) (append (free-variables e1) (free-variables e2))]

      [(If p c a) (append (free-variables p) (free-variables c) (free-variables a))]

      [(Begin args) (append-map free-variables args)]

      ;; For `let` we can't simply append all free variables, precisely because
      ;; `let` binds a value to x. Thus, the free variables in e2 are the free
      ;; variables sans the bound values.
      [(Let x e1 e2)
       (append (free-variables e1)
               (remq* (list x) (free-variables e2)))]

      [(LetRec bindings body)
       (displayln "free-vars matched LetRec")
       ;; Bindings in `let` are a list of lists, where the first element
       ;; is the symbol, so we simply map car
       (let ([bound-vars-symbols (map car bindings)]
             ;; For each binding, check its expression's free variables
             [free-in-values (append-map free-in-let-def bindings)])

         ;; Similar to let, the body's free variables are all of its trivially defined
         ;; free variables sans those that were just bound
         (remq* bound-vars-symbols (append free-in-values (free-variables body))))]

      [(Lambda f params body) (remq* params (free-variables body))]

      [(App expr args) (append (free-variables expr) (append-map free-variables args))]

      [_ '()]))
  (remove-duplicates (free-variables expr)))

;; free-in-let-def: [Symbol Expr] -> [Vars]
;; free-in-let-def takes a list with a symbol and a value
;; (i.e. a binding list in a `let` form) and returns the
;; list of free variables in Expr
(define (free-in-let-def definition)
  (displayln "in free-in-let-def")
  (match definition
    [(list (? symbol? x) v) (free-variables v)]))

;; desugar: Prog -> Prog
;; desugar takes a Prog node and expresses its user defined functions
;; in terms of Lambdas, namely it uses LetRec
(define (desugar e+)
  (match e+
    [(Prog '() e) (Prog '() (desugar e))]
    ;; A program with user defined functions is equivalent to a program
    ;; with no user-defined functions, but a big letrec that has all
    ;; the function definitions and whose body is the program's body
    [(Prog defs e)
     (let ([desugared-defs (map desugar defs)])
       (Prog '() (LetRec desugared-defs (desugar e))))]

    ;; When we convert a Program with Defs to a LetRec program, we'll want the
    ;; structure of the LetRec's definitions to be a list of lists, so this
    ;; gets translated to:
    ;; (letrec
    ;;   ((f (lambda params body))))
    ;;    ^------- this is why we return a list
    [(Defn f params body) (list f (Lambda f params (desugar body)))]

    [(Prim1 p e) (Prim1 p (desugar e))]
    [(Prim2 p e1 e2) (Prim2 p (desugar e1) (desugar e2))]

    [(If p c a) (If (desugar p) (desugar c) (desugar a))]
    [(Cond cs)
     (desugar-cond cs)]
    [(Begin args) (Begin (map desugar args))]

    [(Let x v e) (Let x (desugar v) (desugar e))]

    [(LetRec bindings body)
     (LetRec (map (lambda (b) (map desugar b)) bindings)
             (desugar body))]

    [(Lambda name params body) (Lambda (gensym 'lam) params (desugar body))]

    [(App (Var 'list) args) (desugar-list args)]
    [(App expr args) (App (desugar expr) (map desugar args))]

    ;; Base case
    [_ e+]))

;; desugar-list: [Expr] -> ASM with cons
;; desugar-list takes a list of expressions which represent arguments to
;; the `list` function, and produces a Prim2 node in which conses are chained
(define (desugar-list args)
  (match args
    ['() (Empty)]
    [(cons a as)
     (Prim2 'cons
            (desugar a)
            (desugar-list as))]))

;; desugar-cond: [[Exprs]...] -> (If ...)
;; desugar-cond takes the list of clauses of the cond and rewrites the node
;; as a sequence of If nodes
(define (desugar-cond cs)
  (match cs
    ['() (Bool #f)]
    [(cons (list p es) rest)
     (If (desugar p)
         (desugar (Begin es))
         (desugar-cond rest))]))

;; Lambda labelling
;; Lambdas are anonymous.. to the user. We need to label them so we know where
;; to jump during codegen.

;; label-lambdas: Prog -> Prog
;; label-lambdas traverses the AST and generates a name for each lambda
(define (label-lambdas e)
  (match e
    [(Prog ds e) (Prog (map label-lambdas ds) (label-lambdas e))]
    [(Defn f params body) (Defn f params (label-lambdas body))]
    [(Prim1 p e) (Prim1 p (label-lambdas e))]
    [(Prim2 p e1 e2) (Prim2 p (label-lambdas e1) (label-lambdas e2))]
    [(If p c a) (If (label-lambdas p) (label-lambdas c) (label-lambdas a))]
    [(Begin args) (Begin (map label-lambdas args))]

    [(Let x v e) (Let x (label-lambdas v) (label-lambdas e))]
    [(LetRec bindings body)
     (LetRec (map (lambda (b) (map label-lambdas b)) bindings)
             (label-lambdas body))]

    ;; Generated by parser
    [(Lambda '() params body) (Lambda (gensym 'lam) params (label-lambdas body))]

    ;; User defined
    [(Lambda name params body) (Lambda (gensym name) params (label-lambdas body))]

    [(App expr args) (App (label-lambdas expr) (map label-lambdas args))]

    [_ e]))


;; Lambda collecting
;; Since lambdas can be anywhere in the code, we need to collect them
;; to generate code for them at a specific place. A solution for this
;; is to treat lambdas as we do user defined functions: collecting them
;; and compiling them together in a single chunk

;; get-lambdas: Prog -> [Lambda]
;; get-lambdas traverses the tree and returns a list of
;; lambdas
(define (get-lambdas e)
  (match e
    [(Prog ds e)       (append (append-map get-lambdas ds) (get-lambdas e))]
    [(Defn f xs e)     (get-lambdas e)]
    [(Prim1 p e)       (get-lambdas e)]
    [(Prim2 p e1 e2)   (append (get-lambdas e1) (get-lambdas e2))]
    [(If e1 e2 e3)     (append (get-lambdas e1) (get-lambdas e2) (get-lambdas e3))]
    [(Begin args)      (append-map get-lambdas args)]
    [(Let x e1 e2)     (append (get-lambdas e1) (get-lambdas e2))]
    [(LetRec bs e1)    (append (append-map lambda-in-let-def bs) (get-lambdas e1))]
    [(Lambda n xs e1)  (cons e (get-lambdas e1))]
    [(App f es)        (append (get-lambdas f) (append-map get-lambdas es))]
    [_                 '()]))

;; Analogous to free-in-let-def
(define (lambda-in-let-def definition)
  (match definition
    [(list x e) (get-lambdas e)]))
