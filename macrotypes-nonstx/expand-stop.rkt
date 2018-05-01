#lang racket/base

(provide expand/stop
         expand/#%var
         (for-template stop/error
                       stop/continue))

(module stop racket/base
  (provide stop/error stop/continue)
  (require (for-syntax racket/base))
  (define-syntax stop/error #f)
  (define-syntax-rule (stop/continue form) form))

(require (for-template 'stop)
         syntax/stx)

;; expand/stop : Stx Symbol -> Stx
;;             : Stx Symbol [Listof Id] -> Stx
(define (expand/stop stx ctx [stop-list '()] [int-def-ctx '()])
  (local-expand stx ctx
                (list* #'stop/error #'stop/continue stop-list)
                int-def-ctx))


;; expand/#%var : Stx Symbol [Listof Id] -> Stx
;; expand/#%var : Stx Symbol [Listof Id] IDCish -> Stx
;; Expand stx, but if it comes to one of the xs, put #%var around it

;; #%var is a new interposition point macro for typechecking and
;; expanding variables
(define (expand/#%var stx ctx xs [int-def-ctx '()])
  (define stx* (expand/stop stx ctx xs int-def-ctx))
  (cond
    ;; x where x ∈ xs
    [(and (identifier? stx*) (member stx* xs free-identifier=?))
     (expand/#%var (datum->syntax stx `(#%var ,stx*) stx stx) ctx xs
                   int-def-ctx)]
    ;; (x . rst) where x ∈ xs
    [(and (stx-pair? stx*) (identifier? (stx-car stx*))
          (member (stx-car stx*) xs free-identifier=?))
     (expand/#%var
      (datum->syntax stx `((#%var ,(stx-car stx*)) . ,(stx-cdr stx*)) stx stx)
      ctx
      xs
      int-def-ctx)]
    [else
     stx*]))

