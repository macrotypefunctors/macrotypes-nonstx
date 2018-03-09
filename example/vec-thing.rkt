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
                     "../type-check.rkt"))

(define-type-constructor Vec [len elem-type])
(define-type-constructor Fin [n]) ; natural number in [0,n)

(define-syntax typed-module-begin
  (syntax-parser
    [(_ e:expr ...)
     #`(#%module-begin
        #,@(for/list ([e (in-list (attribute e))])
             (tc-in '() e)))]))

(define-typed-syntax typed-datum
  [⊢≫⇒
   [G ⊢ #'(_ . n:nat)]
   (stx:also-is
    (er ⊢≫⇒ ≫ #''n ⇒ (Fin (add1 (syntax-e #'n))))
    (syntax-e #'n))]
  [⊢≫⇐
   [G ⊢ stx ⇐ τ_exp]
   (ec G ⊢ stx ≫ stx- ⇒ τ_act)
   (unless (equal? τ_exp τ_act)
     (raise-syntax-error #f "type mismatch" stx))
   (er ⊢≫⇐ ≫ stx-)]
  [else
   (define stx this-syntax)
   (syntax-parse stx
     [(_ . n:nat)
      (stx:also-is
       (er ⊢≫⇒ ≫ #''n ⇒ (Fin (add1 (syntax-e #'n))))
       (syntax-e #'n))])])

(define-syntax type-quote
  (syntax-parser
    [(_ datum)
     (type-stx (syntax->datum #'datum))]))

(define-typed-syntax make-vec
  [⊢≫⇒
   [G ⊢ #'(_ n:integer e:expr)]
   (ec G ⊢ #'e ≫ #'e- ⇒ τ_elem)
   (er ⊢≫⇒ ≫ #`(-make-vector n e-) ⇒ (Vec (syntax-e #'n) τ_elem))]
  [⊢≫⇐
   [G ⊢ stx ⇐ τ_exp]
   (ec G ⊢ stx ≫ stx- ⇒ τ_act)
   (unless (equal? τ_exp τ_act)
     (raise-syntax-error #f "type mismatch" stx))
   (er ⊢≫⇐ ≫ stx-)])

(define-typed-syntax vec-ref
  [⊢≫⇒
   [G ⊢ #'(_ vec:expr i:expr)]
   (ec G ⊢ #'vec ≫ vec- ⇒ (Vec len τ_elem))
   (ec G ⊢ #'i ≫ i- ⇐ (Fin len))
   (er ⊢≫⇒ ≫ #`(vector-ref #,vec- #,i-) ⇒ τ_elem)]
  [⊢≫⇐
   [G ⊢ stx ⇐ τ_exp]
   (ec G ⊢ stx ≫ stx- ⇒ τ_act)
   (unless (equal? τ_exp τ_act)
     (raise-syntax-error #f "type mismatch" stx))
   (er ⊢≫⇐ ≫ stx-)])

