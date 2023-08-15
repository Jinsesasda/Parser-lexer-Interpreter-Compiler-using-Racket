#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]
    ;; TODO: Handle cond
     [(Cond clauses else-expr)
     (interp-cond clauses else-expr)]

)
  )

(define (interp-cond clauses else-expr)
  (cond [(null? clauses) (interp else-expr)]  ; No clauses, evaluate else-expr
        [else
         (match (car clauses)
           [(Clause test-expr then-expr)
            (if (interp test-expr)
                (interp then-expr)
                (interp-cond (cdr clauses) else-expr))])]))


