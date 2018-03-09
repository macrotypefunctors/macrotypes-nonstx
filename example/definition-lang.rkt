#lang racket/base

(provide Int
         ->
         (rename-out
          [typed-module-begin #%module-begin]
          [typed-define define]
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
                     (except-in "../type-check.rkt" tc)))

(define-base-type Int)
(define-base-type Bool)
(define-type-constructor -> [in out])

;; --------------------------------------------------------------

;; Dealing with Definitions

(define-syntax typed-module-begin
  (syntax-parser
    [(_ d:expr ...)
     (define-values [module-G revds]
       (for/fold ([G '()]
                  [revds '()])
                 ([d (in-list (attribute d))])
         (ec G ⊢ d ≫ d- def⇒ G*)
         (values G* (cons d- revds))))
     #`(#%module-begin
        #,@(for/list ([d (in-list (reverse revds))])
             (tc-in module-G d)))]))

(define-typed-syntax typed-define
  #:datum-literals [:]
  [⊢≫def⇒
   ; this G will include *only* previous definitions
   ; DO NOT typecheck e in this context
   [G ⊢ #'(_ x : τ-stx e)]
   (define τ (expand-type #'τ-stx))
   (er ⊢≫def⇒ ≫ #`(define/pass-2 x : #,(type-stx τ) e)
       def⇒ (cons (list #'x τ) G))])

(define-syntax-parser define/pass-2
  #:datum-literals [:]
  [(_ x : τ-stx e)
   (match this-syntax
     [(tc-in G _)
      ; this G will include all top-level definitions in the program
      ; e can only be typechecked in *this* G
      (define τ (expand-type #'τ-stx))
      (ec G ⊢ #'e ≫ e- ⇐ τ)
      #`(define x #,e-)])])

;; --------------------------------------------------------------

;; Expressions

(define-typed-syntax typed-datum
  [⊢≫⇒
   [G ⊢ #'(_ . i:integer)]
   (er ⊢≫⇒ ≫ #''i ⇒ (Int))]
  [⊢≫⇒
   [G ⊢ #'(_ . b:boolean)]
   (er ⊢≫⇒ ≫ #''b ⇒ (Bool))])

(define-typed-syntax typed-app
  [⊢≫⇒
   [G ⊢ #'(_ f:expr a:expr)]
   (ec G ⊢ #'f ≫ #'f- ⇒ (-> τ_a τ_b))
   (ec G ⊢ #'a ≫ #'a- ⇐ τ_a)
   (er ⊢≫⇒ ≫ #`(f- a-) ⇒ τ_b)])

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
   (er ⊢≫⇒ ≫ #'x ⇒ τ)])

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

