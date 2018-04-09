#lang racket/base

(provide prop:expand-check-sugar-info
         ;; predicate
         expand-check-sugar-info?
         ;; match expander
         expand-check-sugar-info
         ;; value constructor
         make-expand-check-sugar-info
         ;; interpreting "Sigs"
         sig-interpret
         ;; accessors
         ;; ...
         ;; syntax class
         expand-check-sugar-id
         )

(require racket/list
         racket/match
         syntax/parse
         syntax/parse/class/local-value
         syntax/stx
         "expand-check-info.rkt"
         (for-syntax racket/base
                     syntax/parse))

;; Think of expand-check like type-check by expansion

;; --------------------------------------------------------------

;; Interpreting "Sigs"

;; A Sig is a [Listof SigElem]
;; A SigElem is one of:
;;  - Id
;;  - (list 'in Nat)
;;  - (list 'out Nat)

;; sig-interpret : Sig [StxListof Stx] -> (list [Listof Stx] [Listof Stx])
;; Returns two values:
;;  - in-indexes
;;  - out-indexes
(define (sig-interpret sig stuff)
  (let loop ([sig sig] [stuff stuff] [ins '()] [outs '()])
    (match sig
      ['()
       (unless (stx-null? stuff)
         (raise-syntax-error #f "unexpected term" (stx-car stuff)))
       (list (map second (sort ins < #:key first))
             (map second (sort outs < #:key first)))]
      [(cons `(in ,i) rst)
       (unless (stx-pair? stuff)
         (raise-syntax-error #f "expected more terms" stuff))
       (loop rst
             (stx-cdr stuff)
             (cons (list i (stx-car stuff)) ins)
             outs)]
      [(cons `(out ,i) rst)
       (unless (stx-pair? stuff)
         (raise-syntax-error #f "expected more terms" stuff))
       (loop rst
             (stx-cdr stuff)
             ins
             (cons (list i (stx-car stuff)) outs))]
      [(cons (? identifier? id) rst)
       (unless (stx-pair? stuff)
         (raise-syntax-error #f
           (format "expected more terms, starting with ~a" (syntax-e id))
           stuff))
       (unless (and (identifier? (stx-car stuff))
                    (free-identifier=? id (stx-car stuff)))
         (raise-syntax-error #f
           (format "expected ~a" (syntax-e id))
           (stx-car stuff)))
       (loop rst
             (stx-cdr stuff)
             ins
             outs)])))

;; --------------------------------------------------------------

;; Property, Predicate, and Private Struct

;; property containing ExpandCheckSugarInfoValue
(define-values [prop:expand-check-sugar-info
                expand-check-sugar-info?
                expand-check-sugar-info-ref]
  (make-struct-type-property 'expand-check-sugar-info))

;; ExpandCheckSugarInfo -> ExpandCheckSugarInfoValue
(define (get-expand-check-sugar-info v)
  (cond
    [(expand-check-sugar-info-value? v) v]
    [(expand-check-sugar-info? v)
     (define f (expand-check-sugar-info-ref v))
     (get-expand-check-sugar-info (f v))]
    [else
     (error 'get-expand-check-info
            "expected an ExpandCheckSugarInfo, given ~v"
            v)]))

(struct expand-check-sugar-info-value [ec-id  ; bound to an `expand-check-info`
                                       literals
                                       sig
                                       in-sig
                                       out-sig
                                       implicit-rules]
  #:transparent
  #:property prop:expand-check-sugar-info (λ (this) this))

;; --------------------------------------------------------------

;; Match Expander and Constructor

(define-match-expander expand-check-sugar-info
  (syntax-parser
    [(_ field-pat:expr ...)
     #'(? expand-check-sugar-info?
          (app get-expand-check-sugar-info
               (expand-check-sugar-info-value field-pat ...)))]))

(define (make-expand-check-sugar-info ec-id  ; bound to an `expand-check-info`
                                      literals
                                      sig
                                      in-sig
                                      out-sig
                                      implicit-rules)
  (expand-check-sugar-info-value ec-id
                                 literals
                                 sig
                                 in-sig
                                 out-sig
                                 implicit-rules))

;; --------------------------------------------------------------

;; Accessors

;; TODO: accessors

;; --------------------------------------------------------------

;; Syntax Class

(define-syntax-class expand-check-sugar-id
   #:attributes [expand-check-id
                 expand-check-id.name-in
                 expand-check-id.name-out
                 literals sig in-sig out-sig
                 implicit-rules]
  [pattern name
    #:declare name (local-value expand-check-sugar-info?)
    #:do [(match-define
            (expand-check-sugar-info *ec-id *lits *sig *in-sig *out-sig
                                     *im-rules)
            (attribute name.local-value))]
    #:with expand-check-id:expand-check-id *ec-id
    #:attr literals *lits
    #:attr sig *sig
    #:attr in-sig *in-sig
    #:attr out-sig *out-sig
    #:attr implicit-rules *im-rules])

;; --------------------------------------------------------------

