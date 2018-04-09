#lang racket/base

(provide ~match ~match-pat
         define/match-expander
         define/syntax-info
         (rename-out [match-define* match-define]))

(require racket/match
         syntax/parse
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     "id-transformer.rkt"))

(define-simple-macro (match? pat:expr val:expr)
  (match val [pat #true] [_ #false]))

;; --------------------------------------------------------------

(define-syntax ~match
  (pattern-expander
   (syntax-parser
     #:literals [values]
     [(_ (values ~! pat:expr ...) e:expr)
      #:with [v ...] (generate-temporaries #'[pat ...])
      #'(~and (~do (define-values [v ...] e)) (~match* pat v) ...)]
     [(_ pat:expr e:expr)
      #:with v (generate-temporary #'pat)
      #'(~and (~do (define v e)) (~match* pat v))])))

(define-syntax ~match*
  (pattern-expander
   (syntax-parser
     #:literals [syntax]
     [(_ (syntax spat) v:id)
      #'(~parse spat v)]
     [(_ pat:expr v:id)
      #'(~and (~fail #:unless (match? pat v))
              (~do (match-define pat v)))])))

;; --------------------------------------------------------------

(define-syntax ~match-pat
  (pattern-expander
   (syntax-parser
     [(_ pat:expr)
      #:with s (generate-temporary #'pat)
      #'(~and s (~match pat #'s))])))

;; --------------------------------------------------------------

(define-syntax match-define*
  (syntax-parser
    #:literals [values syntax]
    [(_ (values ~! pat:expr ...) e:expr)
     #:with [v ...] (generate-temporaries #'[pat ...])
     #'(begin
         (define-values [v ...] e)
         (match-define* pat v)
         ...)]
    [(_ (syntax ~! spat) e:expr)
     #'(define/syntax-parse spat e)]
    [(_ pat:expr e:expr)
     #'(match-define pat e)]))

;; --------------------------------------------------------------

(define-syntax define/match-expander
  (syntax-parser
    [(_ name:id fn-expr:expr match-transformer:expr)
     #'(begin
         (define fn-id
           (let ([name fn-expr])
             name))
         (define-match-expander name
           match-transformer
           (var-like-transformer
            (λ (stx) (quote-syntax fn-id)))))]))

(define-syntax define/syntax-info
  (syntax-parser
    [(_ name:id fn-expr:expr get-syntax-info:expr)
     #:with fn-id (generate-temporary #'name)
     #'(begin
         (define fn-id
           (let ([name fn-expr])
             name))
         (define-syntax name
           (get-syntax-info (quote-syntax fn-id))))]))

