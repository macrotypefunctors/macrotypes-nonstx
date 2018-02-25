#lang racket/base

(provide define-base-type
         define-type-constructor
         (rename-out [stop/continue untyped]))

(require syntax/parse/define
         (for-syntax racket/base
                     racket/match
                     "id-transformer.rkt"
                     "expand-stop.rkt"
                     "type-prop.rkt"))

;; (define-base-type name)
;; binds at phase 0:
;;  - name, an identifier macro for the type
;; binds at phase 1:
;;  - name, a struct-id and constructor
;;  - name?, a struct predicate
(define-simple-macro
  (define-base-type name:id)
  (begin
    (begin-for-syntax
      (struct name [] #:prefab))
    (define-syntax name
      (id-transformer (λ (stx) (type-stx (name)))))))

;; (define-type-constructor name [field ...])
;; binds at phase 0:
;;  - name, a macro for the type
;; binds at phase 1:
;;  - name, a struct-id and constructor
;;  - name?, a struct predicate
;;  - name-field ..., struct accessors
(define-simple-macro
  (define-type-constructor name:id [field:id ...])
  (begin
    (begin-for-syntax
      (struct name [field ...] #:prefab))
    (define-syntax name
      (syntax-parser
        [(_ (~var field) ...)
         (match* [(expand/stop (attribute field) 'expression) ...]
           [[(type-stx field) ...]
            (type-stx (name field ...))])]))))

