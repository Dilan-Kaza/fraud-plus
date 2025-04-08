#lang racket
(provide interp)
(provide interp-env)
(require "ast.rkt")
(require "interp-prim.rkt")

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
    [(Lit d) d]
    [(Eof)   eof]
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
    ;; TODO: implement n-ary primitive
    [(PrimN p es) (match (interp*-env es r) 
                        ['err 'err]
                        [es+ (interp-primN p es+)])]
    [(If e0 e1 e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    ;; TODO: implement cond
    [(Cond cs e) (interp-cond cs e r)]
    ;; TODO: implement case
    [(Case ev cs el) (interp-case ev cs el r)]
    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    [(Let x e1 e2) (interp-let x e1 e2 r)]
    ;; TODO: implement let*
    [(Let* xs es e) (interp-let* xs es e r)]))


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
            [vs (cons v vs)])])]
    [_ 'err]))

(define (interp-cond cs e r)
  (match cs
  [(list (Clause b v) t ...)
    (match (interp-env b r)
      ['err 'err]
      [b+ (match (interp-env v r)
            ['err 'err]
            [v+ (match (interp-cond t e r)
                  ['err 'err]
                  [t+ (if b+ v+ t+)])])])]
  ['() (interp-env e r)]
  [_ 'err]))

(define (interp-case ev cs el r)
  (match cs
  ['() (interp-env el r)]
  [(list (Clause ls e) t ...)
    (match (interp-env ev r) 
      ['err 'err]
      [ev+  (match (interp-env e r)
              ['err 'err]
              [e+ (match (interp-case ev t el r)
                ['err 'err]
                [el+ (if (member ev+ ls) e+ el+)])])])]
  [_ 'err]))

(define (interp-let x e1 e2 r)
  (match (interp*-env e1 r)
   ['err 'err]
   [e1+ (match (ext-all r x e1+) 
          ['err 'err]
          [r+ (interp-env e2 r+)])]))

(define (interp-let* xs es e r)
  (match xs
       ['err 'err]
       ['() (interp-env e r)]
       [(list xh xt ...)
        (match es
          [(list eh et ...)
            (match (interp-env eh r)
                ['err 'err]
                [eh+ (interp-let* xt et e (ext r xh eh+))])])]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))


;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

(define (ext-all r x v)
  (match x
  ['err 'err]
  ['() r]
  [(list xh xt ...)
   (match v
    ['err 'err]
    [(list vh vt ...) (ext-all (ext r xh vh) xt vt)])]))
