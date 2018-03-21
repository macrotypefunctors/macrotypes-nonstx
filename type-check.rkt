#lang agile

(provide tc tc/chk tc/def
         tc-in tc/chk-in tc/def-in
         tc-out/stop tc/chk-out/stop tc/def-out/stop
         ⊢ ≫ ⇒ ⇐ def⇒
         ⊢≫⇒
         ⊢≫⇐
         ⊢≫def⇒
         cases
         ec
         er)

(require "expand-check-sugar.rkt")

(define-expand-check-relation tc/def
  [G-in d/e -> d/e- G-out]
  [G-in ⊢ d/e ≫ d/e- def⇒ G-out] ; user
  [G-in ⊢ d/e]                    ; definer-input
  [≫ d/e- def⇒ G-out]            ; definer-output
  #:in-stx d/e
  #:out-stx d/e-
  #:context 'module
  #:stop-ids (map first G-in)
  #:bad-output (raise-syntax-error #f "expected a typed expression" d/e))

(define-expand-check-relation tc/chk
  [G expr type -> expr-]
  [G ⊢ expr ≫ expr- ⇐ type]
  [G ⊢ expr ⇐ type]
  [≫ expr-]
  #:in-stx expr
  #:out-stx expr-
  #:stop-ids (map first G)
  #:bad-output (raise-syntax-error #f "expected a typed expression" expr)
  #:implicit-rule
  [⊢≫def⇒
   [G ⊢ e]
   (er ⊢≫def⇒ ≫ e def⇒ G)])

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
   (er ⊢≫⇐ ≫ expr-)]
  #:implicit-rule
  [⊢≫def⇒
   [G ⊢ e]
   (er ⊢≫def⇒ ≫ e def⇒ G)])

