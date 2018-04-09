#lang racket/base

(provide id-transformer
         var-like-transformer)

(require syntax/stx)

;; id-transformer : [Identifier -> Syntax] -> [Syntax -> Syntax]
(define ((id-transformer trans) stx)
  (cond
    [(identifier? stx) (trans stx)]
    [(and (stx-pair? stx) (identifier? (stx-car stx)))
     (datum->syntax stx
                    (cons (trans (stx-car stx))
                          (stx-cdr stx))
                    stx
                    stx)]
    [else
     (raise-syntax-error #f "bad use of identifier macro" stx)]))

;; var-like-transformer :
;; [Identifier -> Syntax] -> [Syntax -> Syntax]
(define ((var-like-transformer trans) stx)
  (cond
    [(identifier? stx) (trans stx)]
    [(and (stx-pair? stx) (identifier? (stx-car stx)))
     (datum->syntax stx
                    ;; major difference:
                    `(#%app ,(stx-car stx) . ,(stx-cdr stx))
                    stx
                    stx)]
    [else
     (raise-syntax-error #f "bad use of identifier macro" stx)]))

