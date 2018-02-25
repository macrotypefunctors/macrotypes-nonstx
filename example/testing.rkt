#lang racket/base

(provide check-type
         check-fail)

(require syntax/parse/define
         rackunit
         (for-syntax racket/base
                     racket/match
                     rackunit
                     "../expand-stop.rkt"
                     "../type-prop.rkt"
                     "../type-check-sugar.rkt"))

(define-syntax-parser check-type
  #:datum-literals [⇒]
  [(_ e ⇒ τ)
   (te G ⊢ this-syntax)
   (tc G ⊢ #'e ≫ e- ⇒ τ-v)
   (match-define (type-stx τ-v/exp) (expand/stop #'τ 'expression))
   (check-equal? τ-v τ-v/exp)
   (tr ≫ #`(void #,e-) ⇒ #false)]
  [(_ e ⇐ τ)
   (te G ⊢ this-syntax)
   (match-define (type-stx τ-v) (expand/stop #'τ 'expression))
   (tc G ⊢ #'e ≫ e- ⇐ τ-v)
   (tr ≫ #`(void #,e-) ⇒ #false)])

(define-syntax-parser check-fail
  [(_ e)
   (te G ⊢ this-syntax)
   (check-exn exn:fail:syntax?
              (λ ()
                (tc G ⊢ #'e ≫ _ ⇒ _)
                (void)))
   (tr ≫ #'(void) ⇒ #false)])

