#lang agile

(provide (rename-out
          [typed-module-begin #%module-begin]
          [typed-datum #%datum])
         Vec
         Fin
         type-quote
         make-vec
         vec-ref
         require)

(require (prefix-in - racket/base)
         syntax/parse/define
         "../type-macros.rkt"
         (for-syntax racket/base
                     racket/match
                     "../id-transformer.rkt"
                     "../expand-stop.rkt"
                     "../type-prop.rkt"
                     "../prop.rkt"
                     "../type-check-sugar.rkt"))

(define-type-constructor Vec [len elem-type])
(define-type-constructor Fin [n]) ; natural number in [0,n)

(define-syntax typed-module-begin
  (syntax-parser
    [(_ e:expr ...)
     #`(#%module-begin
        #,@(for/list ([e (in-list (attribute e))])
             (in-typed-stx e '())))]))

(define-syntax typed-datum
  (syntax-parser
    [(_ . n:nat)
     (stx:also-is
      (tr ≫ #''n ⇒ (Fin (add1 (syntax-e #'n))))
      (syntax-e #'n))]))

(define-syntax type-quote
  (syntax-parser
    [(_ datum)
     (type-stx (syntax->datum #'datum))]))

(define-syntax make-vec
  (syntax-parser
    [(_ n:integer e:expr)
     (te G ⊢ this-syntax)
     (tc G ⊢ #'e ≫ e- ⇒ τ_elem)
     (tr ≫ #`(-make-vector n #,e-) ⇒ (Vec (syntax-e #'n) τ_elem))]))

(define-syntax vec-ref
  (syntax-parser
    [(_ vec:expr i:expr)
     (te G ⊢ this-syntax)
     (tc G ⊢ #'vec ≫ vec- ⇒ (Vec len τ_elem))
     (tc G ⊢ #'i ≫ i- ⇐ (Fin len))
     (tr ≫ #`(vector-ref #,vec- #,i-) ⇒ τ_elem)]))

