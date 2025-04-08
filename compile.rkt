#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define r9  'r9)  ; scratch
(define rsp 'rsp) ; stack
(define r15 'r15) ; stack pad (non-volatile)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Label 'entry)
        ;; save callee-saved register
        (Push r15)
        (compile-e e '())
        ;; restore callee-save register
        (Pop r15)
        (Ret)
        ;; Error handler
        (Label 'err)
        pad-stack
        (Call 'raise_error)))

;; type CEnv = (Listof [Maybe Id])
;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Lit d)         (compile-value d)]
    [(Eof)           (compile-value eof)]
    [(Var x)         (compile-variable x c)]
    [(Prim0 p)       (compile-prim0 p)]
    [(Prim1 p e)     (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    ;; TODO: implement n-ary primitives
    [(PrimN p es)    (compile-primN p es c)]
    [(If e1 e2 e3)   (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)   (compile-begin e1 e2 c)]
    ;; TODO: this only works for single variable binding,
    ;; make it work in general
    [(Let x e1 e2)
     (compile-let x e1 e2 c)]
    ;; TODO: implement let*, case, cond
    [(Let* xs es e)  (compile-let* xs es e c)]
    [(Case ev cs el) (compile-case ev cs el c)]
    [(Cond cs el)    (compile-cond cs el c)]))


;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 -> Asm
(define (compile-prim0 p)
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

(define (compile-primN p es c)
  (match p
    ['+ (seq (Mov rax (value->bits 0))
             (compile-+N es c))]
    [_ (seq (Jmp 'err))]))

(define (compile-+N es c)
  (match es
    ['() '()]
    [(list e t ...)
     (seq (Push rax)
          (compile-e e (cons #f c))
          (Pop r9)
          (Add rax r9)
          (compile-+N t c))]
    [_ (seq (Jmp 'err))]))

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

(define (compile-cond cs e c)
  (match cs
    [(list (Clause e1 e2) a ...)
     (let (
           (l1 (gensym 'cond))
           (l2 (gensym 'cond)))
          (seq (compile-e e1 c)
               (Cmp rax (value->bits #f))
               (Je l1)
               (compile-e e2 c)
               (Jmp l2)
               (Label l1)
               (compile-cond a e c)
               (Label l2)))]
    ['() (compile-e e c)]
    [_ (Jmp 'err)]))

(define (compile-case e1 cs e2 c)
  (match cs
    [(list (Clause lst e) a ...)
       (let ((l1 (gensym 'case))
             (l2 (gensym 'case)))
             (append (seq (compile-e e1 c)
                          (Mov r9 rax))    
                          (compile-contains lst l1 c)
                          (seq (compile-case e1 a e2 c)
                               (Jmp l2)
                               (Label l1)
                               (compile-e e c)
                               (Label l2))))]
    ['() (compile-e e2 c)]
    [_ (Jmp 'err)]))
  
(define (compile-contains lst jmppnt c)
  (match lst
    [(list e t ...)
                    (append (seq (compile-value e)
                                 (Cmp rax r9)
                                 (Je jmppnt))
                            (compile-contains t jmppnt c))]
      ['() '()]
      [_ (Jmp 'err)]))


;; Id Expr Expr CEnv -> Asm
;; NOTE: this is specialized for a single variable binding
;; You should write another function for the general case
(define (compile-let1 x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

(define (compile-let x e1 e2 c)
  (if (= (length x) (length e1))
      (let ([c+ (add-in-order x c)] [k (count-8 x)]) (seq (compile-e* e1 c)
       (compile-e e2 c+)
       (Add rsp k)))
      (Jmp 'err)))

(define (add-in-order x c)
  (match x
    ['() c]
    [(list h t ...) (add-in-order t (cons h c))]
    [_ (Jmp 'err)]))
(define (count-8 x)
  (match x
    ['() 0]
    [(list h t ...) (+ 8 (count-8 t))]
    [_ (Jmp 'err)]))

(define (compile-let* xs es e2 c)
  (if (= (length xs) (length es))
      (match xs
        ['() (compile-e e2 c)]
        [(list x t ...)
        (match es
          [(list e1 et ...)(seq (compile-e e1 c)
            (Push rax)
            (compile-let t et e2 (cons x c))
            (Add rsp 8))]
          [_ (Jmp 'err)])]
        [_ (Jmp 'err)])
      (Jmp 'err)))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))
