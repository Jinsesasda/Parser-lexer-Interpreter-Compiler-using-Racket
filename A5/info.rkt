#lang info
(define programs
  (list
    '[ (add1 1) ]
    '[ (write-byte 97) ]
    '[ (define (f x) (+ x x)) (f 5) ]
    '[ (define (f x) (+ x x)) (f 5 10) ]  
    '[ (define (g x) (+ x x)) (g) ]
    '[ (define (sub1 1))]
'[(define f
    (case-lambda
      [(x y z . r) (length r)]
      [(x) "just one!"])) (f #t)]



'[(define f
    (case-lambda
      [(x . xs)] "One argument"
      [xs "Two argument"]))]

'[(begin
  (define (sum . xs)
    (if (empty? xs)
        0
        (+ (car xs)
           (apply sum (cdr xs)))))
  (sum 1 2 3 4))]

'[(define programs (list [(add1 0)]))]

'[(define f (case-lambda [(x y) x]
                         [(x y . xs)
                           (apply f xs)]))
                     (f 1 2 (cons 3 (cons 4 '())))]
'[(define (f x y) (+ x y)) (apply f (list 1 2 3))]
'[(define(f x y ) (+x y)) (f 1 2 3 4 5)]



'[(define f (case-lambda [(x) "got one"]
                         [(p q) "Got two!"]))
   (f #t)
   (f #t #f)
   (f #t #f 0)]
'[(define f (case-lambda [(x) "got one"]
                         [(p q) "Got two!"]))
   (f #t)
   
   (f #t #f 0)]

'[(define f (case-lambda [(x) "got one"]
                         [(p q) "Got two!"]))
  
   (f #t #f 0)]


'[(define f (case-lambda [(x) "got one"]
                         [(p q) "Got two!"]))
   (f #f)]

'[(define f (case-lambda [(x) "got one"]
                         [(p q) "Got two!"]))
   (f #f #f)]


'[(define (f x y z . xs) (list x y z xs)) (f)]

'[(define (f x . xs) (list x y z xs)) (f)]
'[(define (f x . xs) (list x xs)) (f)]
'[(define (f x y z . xs) (list x x z xs)) (f 1)]
'[(define (f x y z . xs) (list x x z xs)) (f 1 2 3 4 5 6 7)]
'[(define (f x y z . xs) (list x x z xs)) (f 1 2)]

'[(define (f x y) (+ x y)) (apply f (list 1 2 3))]

'[(define f
                        (case-lambda
                          [(x y . z) z]
                          [(x) (+ x x)]
                          [z 2]))
                     (cons (f 1 2)
                            (cons (f 1)
                                  (cons (f 1 2 3)
                                        '())))]

'[(define (f x) x)
                     (f)]
'[(define (f) 1)
                     (f 2)]


'[(define (f x) x)
                     (let ((y 2))
                        (f 1 y))]


'[(define (f . xs)
                        (if (empty? xs)
                            #t
                            (f)))
                     (f 1 2 3)]

'[(define (f x . xs) xs)
                     (f)]


'[(define f (case-lambda))
                     (f)]
'[(define (f x . xs) xs)
                     (let ((x 3))
                        (f 1 x))
                ]
'[(define f
                        (case-lambda))
                     (add1 8)]
'[(define f
                        (case-lambda
                          [(x) x]
                          [(x . xs)
                           (apply f xs)]))
                     (f)]

'[(define f
                        (case-lambda
                          [(x y) x]
                          [(x y . xs)
                           (apply f xs)]))
                     (f 1 2 3)]

'[(define f
                        (case-lambda
                          [(x y) x]
                          [(x y . xs)
                           (apply f xs)]))
                     (f 1 2 (cons 3 (cons 4 '())))]




    #|
    ;; Miscellaneous
    '[ (add1 1) ]
    '[ (write-byte 97) ]
    '[ (define (f x) (+ x x)) (f 5) ]
    '[(add1 #f)]
    '[(sub1 #f)]
    '[(zero? #f)]
    '[(char->integer #f)]
    '[(integer->char #f)]
    '[(integer->char -1)]
    '[(write-byte #f)]
    '[(write-byte -1)]
    '[(write-byte 256)]

                '[(define (f x) x)
                 '(f 5)]
  
   
                 '[(define (tri x)
                    (if (zero? x)
                        0
                        (+ x (tri (sub1 x)))))
                 '(tri 9)]

   
                 '[(define (even? x)
                    (if (zero? x)
                        #t
                        (odd? (sub1 x))))]
                 '[(define (odd? x)
                    (if (zero? x)
                        #f
                        (even? (sub1 x))))
                 '(even? 101)]
  

   
                 '[(define (map-add1 xs)
                    (if (empty? xs)
                        '()
                        (cons (add1 (car xs))
                              (map-add1 (cdr xs)))))
                 '(map-add1 (cons 1 (cons 2 (cons 3 '()))))]

  ;; Iniquity+
    '[(define (f x) x)
                     '(f)]
    '[(define (f) 1)
                     '(f 2)]
  
  
    '[(define (f x) x)
                     '(let ((y 2))
                        (f 1 y))]
    '[(define (f . xs)
                        (if (empty? xs)
                            #t
                            (f)))
                     '(f 1 2 3)]
 
    '[(define (list . xs) xs)
                     '(list (list) (list 1 2 3) (list #t) (list 3 4 5))]
    '[(define (f x y . z) (cons x (cons y z)))
                     '(cons (f 1 2) (cons (f 8 9 10) '()))]
  
  
    '[(define (f x . xs) x)
                     '(f 2)]
    '[(define (f x . xs) xs)
                     '(f 1)]
  
    '[(define (f x . xs) xs)
                     '(f)]
    '[(define (f x . xs) xs)
                     '(let ((x 3))
                        (f 1 x))]

    '[(define f
                        (case-lambda))
                     '(f)]
    '[(define f
                        (case-lambda))
                     '(add1 8)]

  
    '[(define f   (case-lambda
                          [(x) x]))
                     '(f 1)]


    '[(define (f . xs) (list xs))'(f 1)] |#
    ) 
  )
