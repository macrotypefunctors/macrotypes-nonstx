#lang racket/base

(provide define-expand-check-relation
         ec
         cases
         er)

(require racket/match
         syntax/parse
         syntax/parse/define
         (prefix-in * "expand-check.rkt")
         (prefix-in * "stxparse-match.rkt")
         (only-in "stxparse-match.rkt" ~match-pat ~match)
         (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/string
                     racket/syntax
                     syntax/stx
                     syntax/parse/class/local-value
                     (only-in syntax/parse [attribute @])))

(begin-for-syntax
  (define (append-ids ids)
    (if (stx-null? ids)
        (datum->syntax #f '||)
        (format-id
         (stx-car ids)
         "~a"
         (string-append* (stx-map (compose symbol->string syntax-e) ids)))))

  (struct relation-literal [])
  (struct expand-check-rel-info relation-literal
    [function-id
     literals
     sig
     in-sig
     out-sig
     implicit-rules])

  (define-syntax-class expand-check-rel-id
    #:description "expand-check relation"
    #:attributes [function-id
                  function-id.name-in
                  function-id.name-out
                  literals sig in-sig out-sig
                  implicit-rules]
    [pattern (~or (~var name (local-value expand-check-rel-info?))
                  (~and x:id (~fail (format "name: ~a" (syntax-e #'x)))))
      #:do [(match-define
              (expand-check-rel-info *fn-id *lits *sig *in-sig *out-sig
                                     *im-rules)
              (@ name.local-value))]
      #:with function-id:*expand-check-id *fn-id
      #:attr literals *lits
      #:attr sig *sig
      #:attr in-sig *in-sig
      #:attr out-sig *out-sig
      #:attr implicit-rules *im-rules])

  ;; returns a syntax object that will produce a sig
  (define (find-sig stuff in out)
    #`(list
       #,@
       (for/list ([x (in-list stuff)])
         (cond [(member x in free-identifier=?)
                #`'(in #,(index-of in x free-identifier=?))]
               [(member x out free-identifier=?)
                #`'(out #,(index-of out x free-identifier=?))]
               [else
                #`(quote-syntax #,x)]))))

  ;; sig-interpret : Sig [StxListof Stx] -> (list [Listof Stx] [Listof Stx])
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
  )

(define-syntax-parser define-expand-check-relation
  #:datum-literals [->]
  [(_ fn-name:id
      [in:id ... -> out:id ...]
      [stuff:id ...]
      [in-stuff:id ...]
      [out-stuff:id ...]
      #:in-stx in-stx:id
      #:out-stx out-stx:id
      (~optional (~seq #:context context) #:defaults ([context #''expression]))
      #:stop-ids stop-ids:expr
      #:bad-output bad-output:expr
      (~seq #:implicit-rule implicit-rule)
      ...)

   #:with [literal ...]
   (remove* (stx->list #'[in ... out ...]) (@ stuff) free-identifier=?)

   #:with name (append-ids (@ literal))

   #:with [(~or (~var _ (local-value relation-literal?))
                new-literal:id)
           ...]
   (remove #'name (@ literal) free-identifier=?)

   ;; `≈` means list equality where order does not matter
   ;; `<≈` means subset
   #:do [(define (<≈? as bs [elem=? equal?])
           (define as* (stx->list as))
           (define bs* (stx->list bs))
           (for/and ([a (in-list as*)])
             (member a bs* elem=?)))]

   ;; by the definition of literal,
   ;; [stuff ...] ≈ [literal ... in ... out ...]

   ;; but these must also be true:
   ;; [in-stuff ...] <≈ [literal ... in ...]
   ;; [out-stuff ...] <≈ [literal ... out ...]
   #:fail-unless (<≈? (@ in-stuff) #'[literal ... in ...] free-identifier=?)
   "unexpected item in `in-stuff`"
   #:fail-unless (<≈? (@ out-stuff) #'[literal ... out ...] free-identifier=?)
   "unexpected item in `out-stuff`"

   #:with sig     (find-sig (@ stuff) (@ in) (@ out))
   #:with in-sig  (find-sig (@ in-stuff) (@ in) (@ out))
   #:with out-sig (find-sig (@ out-stuff) (@ in) (@ out))

   #:with [implicit-rule-name ...]
   (map (λ (i) (generate-temporary (format "implicit-rule~a:" i)))
        (range (length (@ implicit-rule))))
   #:with [implicit-rule-def ...]
   #'[(define-implicit-rule-syntax-class implicit-rule-name implicit-rule) ...]
   #:with [implicit-rule-ref ...]
   #'[(quote-syntax implicit-rule-name) ...]

   #'(begin
       (define-syntax new-literal (relation-literal))
       ...
       implicit-rule-def
       ...
       (define-syntax name
         (expand-check-rel-info
          (quote-syntax fn-name)
          (list (quote-syntax literal) ...)
          sig
          in-sig
          out-sig
          (list implicit-rule-ref ...)))

       (*define-expand-check-function fn-name
         [in ... -> out ...]
         #:in-stx in-stx
         #:out-stx out-stx
         #:context context
         #:stop-ids stop-ids
         #:bad-output bad-output)
       )])



;; ec for "expand-check"
(define-syntax-parser ec
  [(_ . stuff)
   #:with [(~or (~var lit (local-value relation-literal?)) _) ...]
   #'stuff
   #:fail-when (and (null? (@ lit)) #'stuff)
   "expected a relation literal somewhere"
   #:with rel:expand-check-rel-id (append-ids (@ lit))
   ;; -------------------------------------------------
   #:with [[in ...] [out ...]] (sig-interpret (@ rel.sig) #'stuff)
   #'(*match-define (values out ...) (rel.function-id in ...))])

;; er for "expanded result"
(define-syntax-parser er
  [(_ rel:expand-check-rel-id . stuff)
   #:with [[] [out ...]] (sig-interpret (@ rel.out-sig) #'stuff)
   #'(rel.function-id.name-out out ...)])

(define-syntax ~ec-in
  (pattern-expander
   (syntax-parser
     [(_ rel:expand-check-rel-id . stuff)
      #:with [[in ...] []] (sig-interpret (@ rel.in-sig) #'stuff)
      #:with [in* ...] (generate-temporaries #'[in ...])
      #:with msg (format "inputs to ~a" (syntax-e #'rel))
      #'(~and (~describe 'msg
                         (~match-pat (rel.function-id.name-in in* ...)))
              (~match in in*)
              ...)])))

;; --------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class case
    #:attributes [[norm 1] [impl 1] [else 1]]
    [pattern [rel:expand-check-rel-id stuff body ...+]
      #:with [norm ...] #'[[(~ec-in rel . stuff) body ...]]
      #:with [impl ...] (@ rel.implicit-rules)
      #:with [else ...] '()]
    [pattern [(~literal else) body ...+]
      #:with [norm ...] '()
      #:with [impl ...] '()
      #:with [else ...] #'[[_ body ...]]]))

(define-syntax-parser cases
  [(_ (~and kw-opt (~not :case)) ... c:case ...)
   #:with [im ...]
   (remove-duplicates (stx->list #'[c.impl ... ...]) free-identifier=?)
   #'(syntax-parser
       kw-opt ...
       c.norm ... ...
       [(~var x im) (attribute x.norm)] ...
       c.else ... ...)])

;; --------------------------------------------------------------

(define-syntax-parser define-implicit-rule-syntax-class
  [(_ name:id [rel:id stuff body:expr ...+])
   #'(define-syntax-class name
       #:attributes [norm]
       [pattern (~ec-in rel . stuff)
         #:attr norm (let () body ...)])])

