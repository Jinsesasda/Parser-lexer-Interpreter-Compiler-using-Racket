#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define rcx 'rcx) ; rcx
(define rdx 'rdx) ; rdx

;; type CEnv = [Listof Variable]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Push rbx)
           (Push r15)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '())
           (Pop r15)
           (Pop rbx)
           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f fun)
     (compile-fun f fun)]))

;; Id Fun -> Asm
(define (compile-fun f fun)
  
  (match fun
    [(FunPlain xs e)
     (let ((expected-args (length xs)))
    
     (seq (Label (symbol->label f))
          ;; Check arity
          (Cmp rcx expected-args)
          (Jne 'raise_error_align)
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs)))
          (Ret)))]

[(FunRest xs x e)
 (let ((required-args (length xs)))
   (seq (Label (symbol->label f))
        ;; Check arit
        (Cmp rcx required-args)
        (Jl 'raise_error_align)
        ;; Calculate the number of rest arguments
       
        ;; Allocate memory for the rest arguments list
        
        (Sub rsp (* 8 required-args)) ; Allocate space for the rest arguments list

        ;; Loop to pop arguments and construct the list
        (Label 'rest_loop)
        (Cmp rcx required-args)
        (Je 'rest_done)
        (Add rsp 8) ; Pop an argument
        (Sub rcx 1)
        (Jmp 'rest_loop)

        ;; Push the rest arguments list as the last argument
        (Label 'rest_done)
         ; Move the address of the rest arguments list to rdx

        ;; Compile the body of the function
        (compile-e e (cons x (list reverse xs)))
     
        ;; Clean up the stack
        (Add rsp (* 8 (+ required-args 1)))
        
        (Ret)))]
    [(FunCase cs)
     (seq)]

 [_ (error "Invalid function type")]
    ))

(define empty-list '())

(define (compile-apply f es c)
  (let ((r (gensym 'ret))
        (num-args (length es)))
    (let* ((apply-args-label (gensym 'apply-args))
           (apply-end-label (gensym 'apply-end))
           (empty-list-label (gensym 'empty-list)))
      (seq (Mov rcx num-args)
           (Lea rax r)
           (Push rax)
           (compile-es es (cons #f c))
           ;; Generate code to handle the list argument
           (Label apply-args-label)
           (Pop rax)
           ;; Check if the list is empty
           (Cmp rax empty-list)
           (Je empty-list-label) ; Jump to the empty list label if the list is empty
           (Push (car rax))      ; Push the current element onto the stack
           (Push (cdr rax))      ; Push the rest of the list
           (Jmp apply-args-label) ; Jump back to handle the next element
           (Label empty-list-label)
           ;; Push a default value onto the stack when the list is empty
           (Push 0)
           ;; Jump to the function's entry point
           (Jmp (symbol->label f))
           (Label r)))))


;; Pattern -> [Listof Variable]
(define (pattern->vars pattern)
  (match pattern
    [(cons x xs) (cons x (pattern->vars xs))]
    ['() '()]))

;; Pattern -> Integer
(define (pattern-arity pattern)
  (length pattern))


 (define (compile-app f es c)
  (let ((r (gensym 'ret))
        (num-args (length es)))
    ;; Check arity
    (seq 
         (Mov rcx num-args)
         
         (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
         
         ;; TODO: communicate argument count to called function
         (Jmp (symbol->label f))
         (Label r))))  

(define (function-args fun)
  (match fun
    [(FunPlain args _) args]
    [_ '()]))



;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]
    [(Str s)            (compile-string s)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(App f es)         (compile-app f es c)]
    [(Apply f es e)     (compile-apply f es e c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

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

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
       (compile-op3 p)))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax (value->bits #f))
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
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame



(define (lookup-function-args f)
  (match f
    [(FunPlain xs _) xs]
    ;; TODO: handle other kinds of functions
    [_ '()]))



;; Id [Listof Expr] Expr CEnv -> Asm


;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))

;; [Listof Expr] CEnv -> Asm
