#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" "mparse.rkt" a86)

;; main: String -> Void
;; So, in summary:
;; 1. String -> S-exps (Reader)
;; 2. S-exps -> AST (parse)
;; 3. AST -> x86 AST (compile)
;; 4. x86 AST -> NASM (asm-string)
(define (main filename)
  (let ([filename-port (open-input-file filename)])
    (begin
      ;; (read-line filename-port) NOTE: uncomment if using the Racket reader
      (displayln
       (map struct->list
        (compile
         (parse
          ;; (read filename-port))))) NOTE: uncomment if using the Racket reader
          (mread (port->string filename-port) filename)))))
      (close-input-port filename-port))))

(define (struct->list quad)
  (match quad
    [(Extern s) `(Extern ,s)]
    [(Label s)  `(Label ,s)]
    [(Mov r1 r2) `(Mov ,r1 ,r2)]
    [(Add r1 r2) `(Add ,r1 ,r2)]
    [(Push r)    `(Push ,r)]
    [(Pop r)     `(Pop ,r)]
    [(And r e)     `(And ,r ,e)]
    [(Cmp r e)   `(Cmp ,r ,e)]
    [(Jmp l)     `(Jmp ,l)]
    [(Je l)      `(Je ,l)]
    [(Jne l)     `(Jne ,l)]
    [(Ret)       `(Ret)]))
