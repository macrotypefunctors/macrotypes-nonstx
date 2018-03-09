#lang racket/base

(provide Int
         ->
         (rename-out
          [typed-module-begin #%module-begin]
          [typed-datum #%datum]
          [typed-app #%app]
          [typed-add1 add1]
          [typed-var #%var]
          [typed-lambda λ]
          )
         require
         )

(require (prefix-in - racket/base)
         syntax/parse/define
         "../type-macros.rkt"
         (for-syntax racket/base
                     racket/match
                     "../id-transformer.rkt"
                     "../expand-stop.rkt"
                     "../type-prop.rkt"
                     "../type-check.rkt"))

(define-base-type Int)
(define-base-type Bool)
(define-type-constructor -> [in out])

(define-syntax typed-module-begin
  (syntax-parser
    [(_ e:expr ...)
     #`(#%module-begin
        #,@(for/list ([e (in-list (attribute e))])
             (tc-in '() e)))]))

(define-typed-syntax typed-datum
  [⊢≫⇒
   [G ⊢ #'(_ . i:integer)]
   (er ⊢≫⇒ ≫ #''i ⇒ (Int))]
  [⊢≫⇒
   [G ⊢ #'(_ . b:boolean)]
   (er ⊢≫⇒ ≫ #''b ⇒ (Bool))]
  [⊢≫⇐
   [G ⊢ stx ⇐ τ_exp]
   (ec G ⊢ stx ≫ stx- ⇒ τ_act)
   (unless (equal? τ_exp τ_act)
     (raise-syntax-error #f "type mismatch" stx))
   (er ⊢≫⇐ ≫ stx-)])

(define-typed-syntax typed-app
  [⊢≫⇒
   [G ⊢ #'(_ f:expr a:expr)]
   (ec G ⊢ #'f ≫ #'f- ⇒ (-> τ_a τ_b))
   (ec G ⊢ #'a ≫ #'a- ⇐ τ_a)
   (er ⊢≫⇒ ≫ #`(f- a-) ⇒ τ_b)]
  [⊢≫⇐
   [G ⊢ stx ⇐ τ_exp]
   (ec G ⊢ stx ≫ stx- ⇒ τ_act)
   (unless (equal? τ_exp τ_act)
     (raise-syntax-error #f "type mismatch" stx))
   (er ⊢≫⇐ ≫ stx-)])

(define-syntax typed-add1
  (var-like-transformer
   (cases
    [⊢≫⇒
     [G ⊢ stx]
     (er ⊢≫⇒ ≫ #'add1 ⇒ (-> (Int) (Int)))])))

(define-typed-syntax typed-var
  [⊢≫⇒
   [G ⊢ #'(_ x:id)]
   (match-define (list _ τ) (assoc #'x G free-identifier=?))
   (er ⊢≫⇒ ≫ #'x ⇒ τ)]
  [⊢≫⇐
   [G ⊢ stx ⇐ τ_exp]
   (ec G ⊢ stx ≫ stx- ⇒ τ_act)
   (unless (equal? τ_exp τ_act)
     (raise-syntax-error #f "type mismatch" stx))
   (er ⊢≫⇐ ≫ stx-)])

(define-typed-syntax typed-lambda
  [⊢≫⇐
   [G ⊢ #'(_ (x:id) body:expr) ⇐ (-> τ_a τ_b)]
   (ec (cons (list #'x τ_a) G) ⊢ #'body ≫ #'body- ⇐ τ_b)
   (er ⊢≫⇐ ≫ #`(lambda (x) body-))]
  [⊢≫⇒
   [G ⊢ #'(_ ([x:id : τ-stx]) body:expr)]
   (define τ_x (expand-type #'τ-stx))
   (ec (cons (list #'x τ_x) G) ⊢ #'body ≫ #'body- ⇒ τ_body)
   (er ⊢≫⇒ ≫ #`(lambda (x) body-) ⇒ (-> τ_x τ_body))])

