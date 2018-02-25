#lang racket/base

(provide
 ;; X -> [StxIs X]
 stx:is
 ;; Stx X -> [StxHas X]
 stx:has
 ;; both of these are also match expanders
 )

(require racket/match
         syntax/parse/define
         "expand-stop.rkt"
         (for-syntax racket/base
                     syntax/transformer))
 
;; --------------------------------------------------------------

(define prop:has 'has)
(define prop:is 'is)

(define (ca*r x)
  (cond [(pair? x) (ca*r (car x))]
        [else      x]))

;; --------------------------------------------------------------

;; A Stx is a syntax object

;; A [StxIs X] is a syntax object which has a property:
;;   prop:is : X
(define (stx:is? v)
  (and (syntax? v) (syntax-property v prop:is) #t))

;; A [StxHas X] is a syntax object which has a property:
;;   prop:has-type : X
(define (stx:has? v)
  (and (syntax? v) (syntax-property v prop:has) #t))

;; --------------------------------------------------------------

;; X -> [Stx:Is X]
(define (x->stx:is-x type)
  (syntax-property #'(stop/error) prop:is type))

;; [Stx:Is X] -> X
(define (stx:is-x->x stx)
  (ca*r (syntax-property stx prop:is)))

;; X -> [Stx:Is X]
;; but also a match expander
(define-match-expander stx:is
  (syntax-parser
    [(_ pat:expr) #'(? stx:is? (app stx:is-x->x pat))])
  (id-transformer (λ (stx) #'x->stx:is-x)))

;; Stx X -> [StxHas X]
(define (stx-x->stx:has-x stx type)
  (syntax-property stx prop:has type))

;; [StxHas X] -> (values Stx X)
(define (stx:has-x->stx/x stx)
  (values (syntax-property stx prop:has #false)
          (ca*r (syntax-property stx prop:has))))

;; Stx X -> [StxHas X]
;; but also a match expander
(define-match-expander stx:has
  (syntax-parser
    [(_ stx-pat:expr x-pat:expr)
     #'(? stx:has? (app stx:has-x->stx/x stx-pat x-pat))])
  (id-transformer (λ (stx) #'stx-x->stx:has-x)))

