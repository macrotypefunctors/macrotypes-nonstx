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
                     "../prop.rkt"
                     "../type-prop.rkt"
                     "../type-check-sugar.rkt"))

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
         (tc G ⊢ d ≫ d- ⇒ G*)
         (values G* (cons d- revds))))
     #`(#%module-begin
        #,@(for/list ([d (in-list (reverse revds))])
             (in-typed-stx d module-G)))]))

(define-syntax-parser typed-define
  #:datum-literals [:]
  [(_ x : τ-stx e)
   ; this G will include *only* previous definitions
   ; DO NOT typecheck e in this context
   (te G ⊢ this-syntax)
   (define τ (expand-type #'τ-stx))
   (tr ≫ #`(define/pass-2 x : #,(type-stx τ) e)
       ⇒ (cons (list #'x τ) G))])

(define-syntax-parser define/pass-2
  #:datum-literals [:]
  [(_ x : τ-stx e)
   ; this G will include all top-level definitions in the program
   ; e can only be typechecked in *this* G
   (te G ⊢ this-syntax)
   (define τ (expand-type #'τ-stx))
   (tc G ⊢ #'e ≫ e- ⇐ τ)
   #`(define x #,e-)])

;; --------------------------------------------------------------

;; Expressions

(define-syntax-parser typed-datum
  [(_ . i:integer)
   (te G ⊢ this-syntax)
   (tr ≫ #''i ⇒ (Int))]
  [(_ . b:boolean)
   (te G ⊢ this-syntax)
   (tr ≫ #''b ⇒ (Bool))])

(define-syntax-parser typed-app
  [(_ f:expr a:expr)
   (te G ⊢ this-syntax)
   (tc G ⊢ #'f ≫ f- ⇒ (-> τ_a τ_b))
   (tc G ⊢ #'a ≫ a- ⇐ τ_a)
   (tr ≫ #`(#,f- #,a-) ⇒ τ_b)])

(define-syntax typed-add1
  (var-like-transformer
   (λ (stx)
     (te G ⊢ stx)
     (tr ≫ #'add1 ⇒ (-> (Int) (Int))))))

(define-syntax-parser typed-var
  [(_ x:id)
   (te G ⊢ this-syntax)
   (match-define (list _ τ) (assoc #'x G free-identifier=?))
   (tr ≫ #'x ⇒ τ)])

(define-syntax-parser typed-lambda
  [(_ (x:id) body:expr)
   #:when (tee? this-syntax)
   (tee G ⊢ this-syntax ⇐ (-> τ_a τ_b))
   (tc (cons (list #'x τ_a) G) ⊢ #'body ≫ body- ⇐ τ_b)
   (tr ≫ #`(lambda (x) #,body-) ⇒ (-> τ_a τ_b))]
  [(_ ([x:id : τ-stx]) body:expr)
   (te G ⊢ this-syntax)
   (match-define (type-stx τ_x) (expand/stop #'τ-stx 'expression))
   (tc (cons (list #'x τ_x) G) ⊢ #'body ≫ body- ⇒ τ_body)
   (tr ≫ #`(lambda (x) #,body-) ⇒ (-> τ_x τ_body))])

