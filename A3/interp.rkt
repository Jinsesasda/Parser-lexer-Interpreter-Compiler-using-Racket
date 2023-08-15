#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Env = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(PrimN p es) 
    (match (interp*-env es r)
     ['err 'err]
     [vs (apply-primN p vs)])]
    
  
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    [(Cond clauses r)
 (interp-cond clauses r)] 
    [(Case ev cs el) (interp-case-clauses (interp ev) cs el)]

    
    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    [(Let (list x) (list e1) e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (ext r x v))])]
 
   ))
       

(define (ext r x v)
  (cons (list x v) r))




(define (apply-op1 op arg)
  (match op
    [`(zero?) (zero? arg)]
    [`(add1) (add1 arg)]
    [`(sub1) (sub1 arg)]
    [else (error "Unknown primitive operation")]))

(define (apply-op2 op arg1 arg2)
  (match op
    [`(+) (+ arg1 arg2)]
    [`(-) (- arg1 arg2)]
    [`(*) (* arg1 arg2)]
    [else (error "Unknown primitive operation")]))


;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*




(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

(define (apply-primN p vs)
(match p 
['+ (if (andmap integer? vs)(apply + vs) 'err) ]
['- (if (andmap integer? vs) (apply - vs) 'err)]
[_ 'err] 
))
;; Env Id Value -> Env



(define (contains-err? vs)
  (ormap (lambda (v) (eq? v 'err)) vs))

(define (interp-cond clauses else-expr)
  (cond [(null? clauses) (interp else-expr)]  ; No clauses, evaluate else-expr
        [else
         (match (car clauses)
           [(Clause test-expr then-expr)
            (if (interp test-expr)
                (interp then-expr)
                (interp-cond (cdr clauses) else-expr))]
           [_ 'err])]))

(define (interp-case-clauses v cs el)
  (match cs
    ['() (interp el)] ; No clauses, evaluate else-expr
  
    [(cons (Clause d1 e2) cs)
      (if (is-in? v d1)
          (interp e2)
          (interp-case-clauses v cs el))]
    [_ 'err]))

(define (is-in? v ds)
  (match ds
    ['() #f]
   [(cons d ds)
    (or (equal? d v)
        (is-in? v ds))]))

