#lang racket/base

(provide
 ;; X -> [StxIs X]
 stx:is
 ;; Stx X -> [StxIs X]
 stx:also-is
 ;; Stx X -> [StxHas X]
 stx:has
 ;; all of these are also match expanders
 )

(require racket/match
         syntax/parse/define
         "expand-stop.rkt"
         (for-syntax racket/base
                     "id-transformer.rkt"))
 
;; --------------------------------------------------------------

(define prop:has 'has)
(define prop:is 'is)

(define (ca*r x)
  (cond [(pair? x) (ca*r (car x))]
        [else      x]))

(struct it [value] #:prefab)

;; --------------------------------------------------------------

;; A Stx is a syntax object

;; A [StxIs X] is a syntax object which has a property:
;;   prop:is : (it X)
(define (stx:is? v)
  (and (syntax? v) (it? (ca*r (syntax-property v prop:is))) #t))

;; A [StxHas X] is a syntax object which has a property:
;;   prop:has-type : (it X)
(define (stx:has? v)
  (and (syntax? v) (it? (ca*r (syntax-property v prop:has))) #t))

;; --------------------------------------------------------------

;; X -> [Stx:Is X]
(define (x->stx:is-x x)
  (stx/x->stx:also-is-x #'(stop/error) x))

;; [Stx:Is X] -> X
(define (stx:is-x->x stx)
  (it-value (ca*r (syntax-property stx prop:is))))

;; Stx X -> [Stx:Is X]
(define (stx/x->stx:also-is-x stx x)
  (syntax-property stx prop:is (it x)))

;; [Stx:Is X] -> (values Stx X)
(define (stx:also-is-x->stx/x stx)
  (values (syntax-property stx prop:is #false)
          (stx:is-x->x stx)))

;; X -> [Stx:Is X]
;; but also a match expander
(define-match-expander stx:is
  (syntax-parser
    [(_ pat:expr) #'(? stx:is? (app stx:is-x->x pat))])
  (id-transformer (λ (stx) #'x->stx:is-x)))

;; Stx X -> [Stx:Is X]
;; but also a match expander
(define-match-expander stx:also-is
  (syntax-parser
    [(_ stx-pat:expr x-pat:expr)
     #'(? stx:is? (app stx:also-is-x->stx/x stx-pat x-pat))])
  (id-transformer (λ (stx) #'stx/x->stx:also-is-x)))

;; Stx X -> [StxHas X]
(define (stx-x->stx:has-x stx x)
  (syntax-property stx prop:has (it x)))

;; [StxHas X] -> (values Stx X)
(define (stx:has-x->stx/x stx)
  (values (syntax-property stx prop:has #false)
          (it-value (ca*r (syntax-property stx prop:has)))))

;; Stx X -> [StxHas X]
;; but also a match expander
(define-match-expander stx:has
  (syntax-parser
    [(_ stx-pat:expr x-pat:expr)
     #'(? stx:has? (app stx:has-x->stx/x stx-pat x-pat))])
  (id-transformer (λ (stx) #'stx-x->stx:has-x)))

