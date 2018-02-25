#lang racket/base

(provide tc tr te tee tee?)

(require racket/match
         syntax/parse/define
         "type-prop.rkt"
         (prefix-in * "type-check.rkt")
         (for-template "type-macros.rkt")
         (for-syntax racket/base))

;; for "type check"
(define-syntax-parser tc
  #:datum-literals [⊢ ≫ ⇐ ⇒]
  ;; in  in   out  out
  [(_ Γ ⊢ e ≫ e- ⇒ τ)
   #'(match-define (out-typed-stx e- τ) (*tc Γ e))]
  ;; in  in   out  in
  [(_ Γ ⊢ e ≫ e- ⇐ τ)
   #'(match-define (out-typed-stx e- _) (*tc/chk Γ e τ))])

;; for "typed result"
(define-syntax-parser tr
  #:datum-literals [≫ ⇒]
  ;;    in   in
  [(_ ≫ e- ⇒ τ)
   #'(out-typed-stx #`(untyped #,e-) τ)])

;; for "type environment"
(define-syntax-parser te
  #:datum-literals [⊢]
  ;; out in
  [(_ Γ ⊢ e)
   #'(match-define (in-typed-stx _ Γ) e)])

;; for "type environment and expected"
(define-syntax-parser tee
  #:datum-literals [⊢ ⇐]
  ;; out in  out
  [(_ Γ ⊢ e ⇐ τ body ...+)
   #'(match-define (in-typed-stx _ Γ τ) e)])

(define (tee? v)
  (match v
    [(in-typed-stx/expect _ _ _) #true]
    [_ #false]))

