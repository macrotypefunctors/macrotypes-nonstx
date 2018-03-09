#lang agile

(provide tc tc/chk)

(require syntax/stx
         "expand-stop.rkt"
         "type-prop.rkt")

;; tc : TypeEnv Stx -> TypedStx
(define (tc env stx)
  (define xs (map first env))
  (define out
    (expand/#%var (in-typed-stx stx env) 'expression xs))
  (match out
    [(out-typed-stx _ _)
     out]
    [_
     (raise-syntax-error #f "expected a typed expression" stx)]))

;; tc/chk : TypeEnv Stx Type -> TypedStx
(define (tc/chk env stx exp-type)
  (define xs (map first env))
  (define out
    (expand/#%var (in-typed-stx/expect stx env exp-type) 'expression xs))
  (match out
    [(out-typed-stx stx- given-type)
     (unless (equal? exp-type given-type)
       (raise-syntax-error #f
         (format "type mismatch:\n  expected: ~v\n  given:    ~v"
                 exp-type given-type)
         stx))
     out]
    [_
     (raise-syntax-error #f "expected a typed expression" stx)]))

