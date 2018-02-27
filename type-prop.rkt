#lang racket/base

(provide
 ;; Type -> TypeStx
 type-stx
 ;; Stx TypeEnv -> InTypedStx
 in-typed-stx
 ;; Stx TypeEnv Type -> InTypedStx
 in-typed-stx/expect
 ;; Stx Type -> OutTypedStx
 out-typed-stx
 ;; all of these are also match expanders
 
 ;; Stx -> Type
 expand-type
 )

(require racket/match
         syntax/parse/define
         (rename-in "prop.rkt" [stx:is type-stx])
         "expand-stop.rkt"
         (for-syntax racket/base
                     "id-transformer.rkt"))

;; --------------------------------------------------------------

;; A Stx is a syntax object

;; A TypeStx is a [StxIs Type]

;; An InTypedStx is a [StxHas InType]
;; An OutTypedStx is a [StxHas OutType]

;; An InType is one of:
;;  - (environment TypeEnv)
;;  - (environment/expect TypeEnv Type)
(struct environment [env] #:prefab)
(struct environment/expect environment [expect] #:prefab)

;; An OutType is a:
;;  (given Type)
(struct given [type] #:prefab)

;; --------------------------------------------------------------

(define (make-in-typed-stx stx env)
  (stx:has stx (environment env)))

(define (make-in-typed-stx/expect stx env type)
  (stx:has stx (environment/expect env type)))

(define (make-out-typed-stx stx type)
  (stx:has stx (given type)))

(define-match-expander in-typed-stx
  (syntax-parser
    [(_ stx-pat:expr env-pat:expr)
     #'(stx:has stx-pat (environment env-pat))])
  (id-transformer (λ (stx) #'make-in-typed-stx)))

(define-match-expander in-typed-stx/expect
  (syntax-parser
    [(_ stx-pat:expr env-pat:expr type-pat:expr)
     #'(stx:has stx-pat (environment/expect env-pat type-pat))])
  (id-transformer (λ (stx) #'make-in-typed-stx/expect)))

(define-match-expander out-typed-stx
  (syntax-parser
    [(_ stx-pat:expr type-pat:expr)
     #'(stx:has stx-pat (given type-pat))])
  (id-transformer (λ (stx) #'make-out-typed-stx)))

(define (expand-type t)
  (match-define (type-stx τ) (expand/stop t 'expression))
  τ)
