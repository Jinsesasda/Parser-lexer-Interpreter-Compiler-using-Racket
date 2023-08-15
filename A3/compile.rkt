#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = (Listof ID)

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Global 'entry)
        (Label 'entry)
        (compile-e e '())
        (Ret)
        (Label 'raise_error_align)
        pad-stack
        (Call 'raise_error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Var x)            (compile-variable x c)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    ;; TODO: implement n-ary primitives
    [(PrimN p es)    (seq)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    ;; TODO: this only works for single variable binding,
    ;; make it work in general
    [(Let (list x) (list e1) e2)
     (compile-let1 x e1 e2 c)]
    ;; TODO: implement let*, case, cond
    [(Let* xs es e)  (seq)]
    [(Case e cs el) (compile-case e cs el)]
    [(Cond cs el)    (compile-clause cs el c)]))



(define (compile-case e cs el)
  (let ((end (gensym 'caseend))
        (ls (map (lambda (_) (gensym 'rhs)) cs)))
    (seq (compile e)
         (compile-datums-comparisons cs ls)
         (compile el)
         (Jmp end)
         (compile-rhss cs end ls)
         (Label end))))
(define (compile-rhss cs end ls)
  (match (cons cs ls)
    [(cons '() '()) (seq)]
    [(cons (cons (Clause _ e) cs) (cons l ls))
     (seq (Label l)
          (compile e)
          (Jmp end)
          (compile-rhss cs end ls))]))

(define (compile-datums-comparisons cs ls)
  (match (cons cs ls)
    [(cons '() '()) (seq)]
    [(cons (cons (Clause ds _) cs) (cons l ls))
     (seq (compile-datums-single ds ls)
          (compile-datums-comparisons cs ls))]))

(define (compile-datums-single ds l)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (Cmp 'rax (value->bits d))
          (Je l)
          (compile-datums-single ds l))]))
  

(define (compile-clause clauses else-expr c)
  
  (let ((else-label (gensym 'else))
        (end-label (gensym 'end)))
    (cond
      [(null? clauses)
       (seq (compile-e else-expr c)
            (Jmp end-label)
            (Label else-label)
            (Label end-label))]
      [else
       (let ((clause (car clauses))
             (remaining-clauses (cdr clauses)))
         (match clause
           [(Clause test-expr then-expr)
            (let ((then-label (gensym 'then)))
              (seq (compile-e test-expr c)
                   (Cmp 'rax val-true)
                   (Je then-label)
                   (Jmp else-label)
                   (Label then-label)
                   (compile-e then-expr c)
                   (Jmp end-label)
                   (Label else-label)
                   (compile-clause remaining-clauses else-expr c)
                   (Label end-label)))]))])))






 
;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))


;; HINT: Another potentially helpful function that
;; emits code to execute each expression and push
;; all the values on to the stack, analogous to interp*-env

;; [Listof Expr] CEnv -> Asm
(define (compile-e* es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-e* es (cons #f c)))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
;; NOTE: this is specialized for a single variable binding
;; You should write another function for the general case
(define (compile-let1 x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))
