#lang racket
(provide (all-defined-out))



;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Prim1 Op Expr)
;; | (If Expr Expr Expr)


;; type Op = 'add1 | 'sub1 | 'zero? | 'abs | '- | 'not |



(struct Int (i)       #:prefab)
(struct Bool (b)      #:prefab)
(struct Prim1 (p e)   #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Cond (cs e)    #:prefab)
(struct Case (e cs el) #:prefab)
(struct Clause (p b)   #:prefab)


