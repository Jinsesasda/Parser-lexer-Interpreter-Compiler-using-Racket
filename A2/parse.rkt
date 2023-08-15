#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [(? boolean?) (Bool s)]
    [(list 'add1 e)  (Prim1 'add1 (parse e))]
    [(list 'sub1 e)  (Prim1 'sub1 (parse e))]
    [(list 'abs e) (Prim1 'abs (parse e))]
    [(list '- e) (Prim1 '- (parse e))]
    [(list 'not e) (Prim1 'not (parse e))]
    [(list 'else e) (parse e)]
       
    [(list 'zero? e) (Prim1 'zero? (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]

    [(list 'cond clauses ...) (parse-cond clauses)]
    

  
    ;; TODO: Handle cond
 
    ;; TODO: Remove this clause once you've added clauses for
    ;; parsing cond and case; it's here just so running the test suite
    ;; doesn't trigger parse errors.
    [_ (Int 0)]
    [_ (error "parse error")]))

(define (parse-cond clauses)
  (Cond (parse-clauses clauses) (parse-else clauses)))

(define (parse-else clauses)
  (let loop ((clauses clauses))
    (cond
      [(null? clauses) (error "Missing else clause in cond expression")]
      [(eq? (caar clauses) 'else) (parse (cadar clauses))]
      [else (loop (cdr clauses))])))
(define (parse-clauses clauses)
  (let ((filtered-clauses (filter (lambda (clause) (not (eq? (car clause) 'else))) clauses)))
    (map parse-clause filtered-clauses)))

(define (parse-clause clause)
  (match clause
    [(list lhs rhs) (Clause (parse lhs) (parse rhs))]
  )
  )
