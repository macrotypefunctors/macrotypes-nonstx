#lang agile

(provide tc tc/chk)

(require syntax/stx
         "expand-stop.rkt"
         "type-prop.rkt")

(define (expand/#%var stx ctx xs)
  (define stx* (expand/stop stx ctx xs))
  (cond
    [(and (identifier? stx*) (member stx* xs free-identifier=?))
     (expand/#%var (datum->syntax stx `(#%var ,stx*) stx stx) ctx xs)]
    [(and (stx-pair? stx*) (identifier? (stx-car stx*))
          (member (stx-car stx*) xs free-identifier=?))
     (expand/#%var
      (datum->syntax stx `((#%var ,(stx-car stx*)) . ,(stx-cdr stx*)) stx stx)
      ctx
      xs)]
    [else
     stx*]))

;; tc : TypeEnv Stx -> TypedStx
(define (tc env stx)
  (define xs (map first env))
  (define out
    (expand/#%var (in-typed-stx stx env) 'expression xs))
  (match out
    [(out-typed-stx _ _)
     out]))

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
     out]))

