#lang racket
(provide mread)
(require
 megaparsack
 megaparsack/text
 data/monad
 data/applicative)

;; TODO:
;; Tokenize calls where whitespace should be valid

;; This Racket file includes a version
;; of the hand-made parser for Lonnrot
;; (i.e. one that does not rely on the
;; Racket reader)
;;
;; All of the parsers that are defined here
;; must produce a S-exp, hence the use of
;; `do` and `pure`

;; Token/Parse
;; junk :: Parser ()
;; junk munches whitespace.
(define junk/p
  (many/p space/p))

;; token :: Parser a -> Parser a
;; token applies junk after applying a parser
(define (token/p p)
  (do [res <- p]
      junk/p

    (pure res)))

;; parse* :: Parser a -> Parser a
;; parse* applies junk before applying a parser
;; as a hack, we finish off with eof/p, to ensure there's a legal program
;; and nothing more
(define (parse*/p p)
  (do
    junk/p
    [prog <- p]
    eof/p
    (pure prog)))

;; Defines a parser that will try to match a pair
;; of parens and something parseable in between
(define (lst/p p)
  (do
    (token/p (char/p #\())
    [res <- (token/p p)]
    (token/p (char/p #\)))
    (pure res)))

;; CONSTANTS
(define scheme-char/p
  (do (char/p #\#) (char/p #\\) any-char/p))

(define boolean/p
  (do (char/p #\#)
      [bool <- (or/p (try/p (char/p #\f))
                     (try/p (char/p #\t)))]

    (pure (match bool
            [#\t #t]
            [#\f #f]))))

(define constant/p
  (or/p integer/p
        (try/p boolean/p)
        scheme-char/p))

;; EXPRESSION

;; We allow +/- to be an initial, which more closely resembles
;; Racket behavior. There is a tradeoff here:
;; - We can more easily spit out the symbol for + and -, instead of
;;   using chars. This is generally true for any special symbolic character,
;;   so that means that primitives and bound functions are structurally the same
;; - But we may clobber primitives (!!!), e.g. if we do (define + 3), then
;;   in the current environment + is no longer a procedure
(define initial/p
  (or/p letter/p
        (char-in/p "+-!$&*/:<=>?~_^")))

(define subsequent/p
  (or/p initial/p
        digit/p
        (char-in/p ".+-@")))

(define variable/p
  (do
      [var <- (list/p
               initial/p
               (many/p subsequent/p))]

      (pure (string->symbol
             (list->string
              (cons (car var) (cadr var)))))))

(define formals/p
  (do [fs <- (list/p
              (char/p #\()
              (many/p (token/p variable/p))
              (char/p #\)))]

      ;; The second parser, many/p produces a list of
      ;; the results of (token/p variable/p)
      ;; A cheap trick really but beggars can't be choosers
      (pure (cadr fs))))

;; (port->string file-port), unlike read, will not produce
;; quoted data, so quotes appear in the string literally as '().
;; Therefore, to avoid changing parse.rkt, we produce ''(), which
;; pattern matches there
(define quote/p
  (do
      (token/p (string/p "quote"))
      ;; Right now, we only support quote to produce the empty list
      ;; [e <- expr/p]
      (token/p (string/p "()"))
    (pure ''())))

(define lambda/p
  (do
    (token/p (string/p "lambda"))
    [params <- (token/p formals/p)]
    [body <- expr/p]
  (pure `(lambda ,params ,body))))

;; bindings: Parser x Int x Int -> Parser
;; bindings parses a list of 2-lists. Since let (for the moment)
;; only takes one binding, while letrec takes multiple, this way we
;; can specify how many bindings per primitive
;;
;; Likewise, we pass a parser, p, because we would like to enforce
;; the fact that `letrec` is implemented as `fix`, i.e. it only binds
;; functions
(define (bindings/p p min max)
  (do
      [bs <-
          (lst/p
           (many/p
            (lst/p (do
                       [var <- (token/p variable/p)]
                       [val <- (token/p p)]
                     (pure (list var val))))
            #:min min
            #:max max))]

      (pure bs)))

;; NOTE: Right now, the compiler only supports one binding for let.
;; If the parser generates a multi-binding let, it will be syntactically
;; valid, but then parse.rkt interprets this as a function application.
;; This is error is caught when `let` is looked up, but the resulting error
;; is cryptic compared to limiting it from here
(define let/p
  (do
    (token/p (string/p "let"))
    [bindings <- (token/p (bindings/p expr/p 1 1))]
    [body <- (token/p expr/p)]
    (pure `(let ,bindings, body))))

(define letrec/p
  (do
    (token/p (string/p "letrec"))
    [bindings <- (bindings/p (lst/p lambda/p) 1 +inf.0)]
    [body <- (token/p expr/p)]

    (pure `(letrec ,bindings ,body))))

(define ops0 '(read-byte peek-byte void))
(define prim0/p
  (do
    [prim <- (guard/p (token/p variable/p)
                      (lambda (t) (member t ops0)))]
    (pure (list prim))))

(define ops1 '(add1 sub1 zero? char? eof-object? integer->char char->integer write-byte box unbox car cdr))
(define prim1/p
  (do
     [prim <- (guard/p (token/p variable/p)
                       (lambda (t) (member t ops1)))]
     [arg <- (token/p expr/p)]

    (pure (list prim arg))))

;; NOTE:
;; Begin was treated as a separate operation in the course
;; We would ideally like to have an n-ary begin, but for now
;; this also works
(define ops2 '(eq? + - * cons < > =))
(define prim2/p
  (do
    [prim <- (token/p (guard/p variable/p (lambda (t) (member t ops2))))]
    [arg1 <- (token/p expr/p)]
    [arg2 <- (token/p expr/p)]

    (pure (list prim arg1 arg2))))

;; TODO: fix: this allows a primitive-n to have n-1 args
(define prim/p
  (apply or/p
   (map try/p (list prim0/p prim1/p prim2/p))))

(define if/p
  (do
    (token/p (string/p "if"))
    [p <- (token/p expr/p)]
    [c <- (token/p expr/p)]
    [a <- (token/p expr/p)]

    (pure `(if ,p ,c ,a))))


(define def/p
  (do
     (token/p (string/p "define"))
      [defs <- (token/p (lst/p
                         (many/p (token/p variable/p) #:min 1)))]
     [body <- (token/p expr/p)]
      (pure `(define ,defs ,body))))

(define begin-defs/p
  (do
      (token/p (string/p "begin"))
      [defs <- (many/p (try/p (lst/p def/p)) #:min 1)]
      [e <- (token/p expr/p)]

      (pure `(begin ,@defs ,e))))

(define app/p
  (do
      [f <- (token/p variable/p)]
      [args <- (many/p (token/p expr/p))]

    (pure `(,f ,@args))))

;; TODO:
;; Currently, we try/p for whole parsers, which means that
;; if they fail, the error diagnoses that an unexpected ( was found
;; We would like this error to say rather that the identifier was not found
(define expr/p
  (or/p constant/p
        variable/p
        (lst/p
         (or/p
          (try/p begin-defs/p)
          quote/p
          (try/p if/p)
          (try/p letrec/p) ;; ambiguity with `let`
          let/p
          lambda/p
          prim/p
          app/p))))

;; DEFINITION
;; TODO: not yet supported in compiler
;; (define define/p
;;   (do
;;     (char/p #\()
;;     (string/p "define")
;;     variable/p
;;     expr/p))

;; Form
(define form/p
  ;; TODO: define
  (do [e <- expr/p]
      (pure e)))

;; Program
(define program/p
  (do
    ;; [p <- (parse*/p (many/p (token/p form/p)))] ;; NOTE: multiple expressions
    [p <- (token/p form/p)] ;; NOTE: As is
    (pure p)))

;; mread: String -> Sexp
;;
;; `mread` takes a `source` string, which is the
;; String representation of a Lonnrot program, and
;; produces its s-exp representation
;;
;; It is called mread in following the reader/expander
;; terminology; the m is for megaparsack.
(define (mread source [filename ""])
  (parse-result!
   (parse-string (parse*/p program/p) source filename)))

(define (my-reader filename)
  (let ([filename-port (open-input-file filename)])
    (port->string filename-port)))

;; (call-with-input-file "input" solution)
