#lang agile

(provide tc tc/chk
         tc-in tc/chk-in
         tc-out/stop tc/chk-out/stop
         ⊢ ≫ ⇒ ⇐
         ⊢≫⇒
         ⊢≫⇐
         cases
         ec
         er)

(require "expand-check-sugar.rkt")

(define-expand-check-relation tc/chk
  [G expr type -> expr-]
  [G ⊢ expr ≫ expr- ⇐ type]
  [G ⊢ expr ⇐ type]
  [≫ expr-]
  #:in-stx expr
  #:out-stx expr-
  #:stop-ids (map first G)
  #:bad-output (raise-syntax-error #f "expected a typed expression" expr))

(define-expand-check-relation tc
  [G expr -> expr- type]
  [G ⊢ expr ≫ expr- ⇒ type]
  [G ⊢ expr]
  [≫ expr- ⇒ type]
  #:in-stx expr
  #:out-stx expr-
  #:stop-ids (map first G)
  #:bad-output (raise-syntax-error #f "expected a typed expression" expr)
  #:implicit-rule
  [⊢≫⇐
   [G ⊢ expr ⇐ τ-expected]
   (ec G ⊢ expr ≫ expr- ⇒ τ-actual)
   (unless (equal? τ-actual τ-expected)
     (raise-syntax-error #f
       (format "type mismatch\n  expected: ~v\n  given:    ~v"
               τ-expected τ-actual)
       expr))
   (er ⊢≫⇐ ≫ expr-)])

