#lang racket/base

(provide define-expand-check-function
         cases
         (for-syntax expand-check-id))

(require (except-in racket/match match-define)
         syntax/parse
         syntax/parse/define
         "prop.rkt"
         "expand-stop.rkt"
         "stxparse-match.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/syntax
                     "expand-check-info.rkt"
                     "id-transformer.rkt"))

;; Think of expand-check like type-check by expansion

(begin-for-syntax
  (define (remove-index lst i)
    (define-values [a b]
      (split-at lst i))
    (append a (rest b))))

(begin-for-syntax
  (define-syntax-class (ec-function-decl name)
    #:attributes [
                  [def 1]            ; phase 0 definitions
                  [function-expr 0]  ; phase 0 expression
                  [ec-info-expr 0]   ; phase 1 expression
                  ]

    #:datum-literals [->]

    [pattern [[in:id ... -> out:id ...]
              #:in-stx in-stx:id
              #:out-stx out-stx:id
              (~optional (~seq #:context context)
                         #:defaults ([context #''expression]))
              #:stop-ids stop-ids:expr
              #:bad-output bad-output:expr]
     #:with name-in (format-id name "~a-in" name)
     #:with name-out (format-id name "~a-out" name)
     #:with expand/name-in (format-id name "expand/~a-in" name)
     #:with name-in-prop (format-id #'here "~a-in-prop" name)
     #:with name-out-prop (format-id #'here "~a-out-prop" name)
     #:with [in-fld ...] (remove #'in-stx (attribute in) free-identifier=?)
     #:with [out-fld ...] (remove #'out-stx (attribute out) free-identifier=?)
     #:with N-in (length (attribute in))
     #:with N-out (length (attribute out))
     #:with in-stx-index (index-of (attribute in) #'in-stx free-identifier=?)
     #:with out-stx-index (index-of (attribute out) #'out-stx free-identifier=?)
     ;; -------------------------------------------------------------------
     #:with [def ...]
     #'[(struct name-in-prop [in-fld ...] #:prefab)
        (struct name-out-prop [out-fld ...] #:prefab)
        (define/match-expander name-in
          (λ (in ...) (stx:has in-stx (name-in-prop in-fld ...)))
          (make-expand-check-match-transformer
           #'name-in-prop 'N-in 'in-stx-index #f))
        (define/match-expander name-out
          (λ (out ...) (stx:has (wrap-syntax/stop out-stx)
                                (name-out-prop out-fld ...)))
          (make-expand-check-match-transformer
           #'name-out-prop 'N-out 'out-stx-index #'wrap-syntax/stop))
        (define (expand/name-in in ...)
          (expand/#%var (name-in in ...) context stop-ids))]
     #:with function-expr
     #'(λ (in ...)
         (match (expand/name-in in ...)
           [(name-out out ...)
            (values out ...)]
           [_
            bad-output]))
     #:with ec-info-expr
     #'(make-expand-check-info
        #'name-in
        'N-in
        #'name-out
        'N-out
        #'expand/name-in)]))

(define-syntax-parser define-expand-check-function
  [(_ name:id . (~var decl (ec-function-decl #'name)))
   #'(begin
       decl.def
       ...
       (define/syntax-info name
         decl.function-expr
         (λ (internal-name)
           (expand-check-info*
            internal-name
            decl.ec-info-expr))))])



(begin-for-syntax
  (struct expand-check-info* [internal-name ec-info-value]
    #:transparent
    #:property prop:expand-check-info
    (λ (this)
      (expand-check-info*-ec-info-value this))
    #:property prop:procedure
    (λ (this stx)
      (define internal-name (expand-check-info*-internal-name this))
      ((var-like-transformer (λ (id) internal-name)) stx)))

  (define-syntax-class case
    [pattern [(~literal else) e:expr ...+]
      #:with norm
      #'[_ e ...]]
    [pattern [(name:expand-check-id in:expr ...)
              e:expr ...+]
      #:with [in* ...] (generate-temporaries #'[in ...])
      #:with [out ...] (generate-temporaries (range (attribute name.N-out)))
      #:with norm
      #'[(~and (~match-pat (name.name-in in* ...))
               (~match in in*)
               ...)
         e ...]]))

(define-syntax-parser cases
  [(_ (~and kw-opt (~not :case)) ... c:case ...)
   #'(lambda (stx)
       (syntax-parse stx kw-opt ... c.norm ...))])

(begin-for-syntax
  (define (make-expand-check-match-transformer prop-struct N stx-i wrap-stx)
    (lambda (stx)
      (syntax-parse stx
        [(_ in:expr ...)
         #:fail-unless (= (length (attribute in)) N)
         (format "expected ~v subpatterns" N)
         #:with stx-pat (list-ref (attribute in) stx-i)
         #:with stx-pat/wrapped (if wrap-stx #`(#,wrap-stx stx-pat) #'stx-pat)
         #:with [fld-pat ...] (remove-index (attribute in) stx-i)
         #`(stx:has stx-pat/wrapped (#,prop-struct fld-pat ...))]))))

(define/match-expander wrap-syntax/stop
  (λ (stx) #`(stop/continue #,stx))
  (syntax-parser
    [(_ stx-pat) #`(app unwrap-syntax/stop stx-pat)]))

(define (unwrap-syntax/stop wstx)
  (syntax-parse wstx
    #:literals [stop/continue]
    [(stop/continue stx) #'stx]
    [stx #'stx]))

#|
(define-expand-check-function tc
  [G expr -> expr- type]
  #:in-stx expr
  #:out-stx expr-
  #:stop-ids (map first G))

(define-expand-check-function tc/chk
  [G expr type -> expr-]
  #:in-stx expr
  #:out-stx expr-
  #:stop-ids (map first G))
|#

