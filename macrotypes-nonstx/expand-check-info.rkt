#lang racket/base

(provide prop:expand-check-info
         ;; predicate
         expand-check-info?
         ;; match expander
         expand-check-info
         ;; value constructor
         make-expand-check-info
         ;; accessors
         expand-check-info-name-in
         expand-check-info-N-in
         expand-check-info-name-out
         expand-check-info-N-out
         expand-check-info-expand/name-in
         ;; syntax class
         expand-check-id
         )

(require racket/match
         syntax/parse
         syntax/parse/class/local-value
         (for-syntax racket/base
                     syntax/parse))

;; Think of expand-check like type-check by expansion

;; --------------------------------------------------------------

;; Property, Predicate, and Private Struct

;; property containing ExpandCheckInfoValue
(define-values [prop:expand-check-info
                expand-check-info?
                expand-check-info-ref]
  (make-struct-type-property 'expand-check-info))

;; ExpandCheckInfo -> ExpandCheckInfoValue
(define (get-expand-check-info v)
  (cond
    [(expand-check-info-value? v) v]
    [(expand-check-info? v)
     (define f (expand-check-info-ref v))
     (get-expand-check-info (f v))]
    [else
     (error 'get-expand-check-info
            "expected an ExpandCheckInfo, given ~v"
            v)]))

(struct expand-check-info-value [name-in N-in
                                 name-out N-out
                                 expand/name-in]
  #:transparent
  #:property prop:expand-check-info (λ (this) this))

;; --------------------------------------------------------------

;; Match Expander and Constructor

(define-match-expander expand-check-info
  (syntax-parser
    [(_ field-pat:expr ...)
     #'(? expand-check-info?
          (app get-expand-check-info
               (expand-check-info-value field-pat ...)))]))

(define (make-expand-check-info name-in N-in
                                name-out N-out
                                expand/name-in)
  (expand-check-info-value name-in N-in
                           name-out N-out
                           expand/name-in))

;; --------------------------------------------------------------

;; Accessors

(define (expand-check-info-name-in v)
  (expand-check-info-value-name-in (get-expand-check-info v)))
(define (expand-check-info-N-in v)
  (expand-check-info-value-N-in (get-expand-check-info v)))
(define (expand-check-info-name-out v)
  (expand-check-info-value-name-out (get-expand-check-info v)))
(define (expand-check-info-N-out v)
  (expand-check-info-value-N-out (get-expand-check-info v)))
(define (expand-check-info-expand/name-in v)
  (expand-check-info-value-expand/name-in (get-expand-check-info v)))

;; --------------------------------------------------------------

;; Syntax Class

(define-syntax-class expand-check-id
  [pattern name
    #:declare name (local-value expand-check-info?)
    #:and ~!
    #:do [(match-define
            (expand-check-info *name-in  *N-in
                               *name-out *N-out
                               *expand/name-in)
            (attribute name.local-value))]
    #:attr name-in *name-in
    #:attr name-out *name-out
    #:attr expand/name-in *expand/name-in
    #:attr N-in *N-in
    #:attr N-out *N-out])

;; --------------------------------------------------------------

