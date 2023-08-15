#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast "parse.rkt")

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)           (compile-integer i)]
    [(Bool b)          (compile-boolean b)]
    [(Prim1 p e)       (compile-prim p e)]
    [(If e1 e2 e3)     (compile-if e1 e2 e3)]
    [(Cond clauses else-expr)    (compile-clause clauses else-expr)]
    ;; TODO: Handle cond
  
    ))



(define (compile-clause clauses else-expr)
  
  (let ((else-label (gensym 'else))
        (end-label (gensym 'end)))
    (cond
      [(null? clauses)
       (seq (compile-e else-expr)
            (Jmp end-label)
            (Label else-label)
            (Label end-label))]
      [else
       (let ((clause (car clauses))
             (remaining-clauses (cdr clauses)))
         (match clause
           [(Clause test-expr then-expr)
            (let ((then-label (gensym 'then)))
              (seq (compile-e test-expr)
                   (Cmp 'rax val-true)
                   (Je then-label)
                   (Jmp else-label)
                   (Label then-label)
                   (compile-e then-expr)
                   (Jmp end-label)
                   (Label else-label)
                   (compile-clause remaining-clauses else-expr)
                   (Label end-label)))]))])))


   





(define (else-only else-expr)
  (seq (compile-e else-expr)
       (let ((l1 (gensym 'end)))
            (seq (Label l1)))))
  
          
;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax (value->bits i))))

;; Boolean -> Asm
(define (compile-boolean b)
  (seq (Mov 'rax (value->bits b))))

;; Op Expr -> Asm
(define (compile-prim p e)
  
  (seq (compile-e e)
       (match p
         ['add1 (Add 'rax (value->bits 1))]
         ['sub1 (Sub 'rax (value->bits 1))]
         ;; TODO: Handle abs, -, and not
         ['abs
          (let ((l1 (gensym 'abs)))
            (seq (Cmp 'rax 0)
                 (Jge l1)
                 (Mov 'rbx 'rax)
                 (Sub 'rax 'rbx)
                 (Sub 'rax 'rbx)
                 (Label l1)))]
         
         ['-
          (let ((l1 (gensym 'neg)))
            (seq (Mov 'rbx 'rax)
                 (Sub 'rax 'rbx)
                 (Sub 'rax 'rbx)))]
         
         ['not
          (let ((l1 (gensym 'not)))
            (seq (Cmp 'rax val-false)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]

         ['zero?
          (let ((l1 (gensym 'nzero)))
            (seq (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))])))




;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))





