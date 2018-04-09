#lang racket/base

(provide
 ;; Type -> TypeStx
 type-stx
 ;; this is also a match expander
 
 ;; Stx -> Type
 expand-type
 )

(require racket/match
         (rename-in "prop.rkt" [stx:is type-stx])
         "expand-stop.rkt")

;; --------------------------------------------------------------

;; A Stx is a syntax object

;; A TypeStx is a [StxIs Type]

;; --------------------------------------------------------------

(define (expand-type t)
  (match-define (type-stx τ) (expand/stop t 'expression))
  τ)
