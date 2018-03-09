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
                     (only-in
                      "../type-check.rkt"
                      ⊢ ≫ ⇒ ⇐ ⊢≫⇒ ⊢≫⇐ cases ec er)))

(define-syntax check-type
  (cases
   #:datum-literals [⇒ ⇐]
   [⊢≫⇒
    [G ⊢ #'(_ e ⇒ τ)]
    (ec G ⊢ #'e ≫ #'e- ⇒ τ-v)
    (define τ-v/exp (expand-type #'τ))
    (check-equal? τ-v τ-v/exp)
    (er ⊢≫⇒ ≫ #`(void e-) ⇒ #false)]
   #;[⊢≫⇒
    [G ⊢ #'(_ e ⇐ τ)]
    (define τ-v (expand-type #'τ))
    (ec G ⊢ #'e ≫ #'e- ⇐ τ-v)
    (er ⊢≫⇒ ≫ #`(void e-) ⇒ #false)]))

(define-syntax check-fail
  (cases
   [⊢≫⇒
    [G ⊢ #'(_ e)]
    (check-exn exn:fail:syntax?
               (λ ()
                 (ec G ⊢ #'e ≫ _ ⇒ _)
                 (void)))
    (er ⊢≫⇒ ≫ #'(void) ⇒ #false)]))

