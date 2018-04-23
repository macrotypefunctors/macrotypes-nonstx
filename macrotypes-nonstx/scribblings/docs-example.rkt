#lang agile

(require macrotypes-nonstx/type-macros
         (prefix-in rkt: racket/base)
         (for-syntax racket/base
                     racket/list
                     racket/dict
                     macrotypes-nonstx/expand-check-sugar))

(begin-for-syntax
  (define-expand-check-relation tc-e
    [G expr -> expr- type]
    [G ⊢e expr ≫ expr- ⇒ type]
    [G ⊢e expr]
    [≫ expr- ⇒ type]
    #:in-stx expr
    #:out-stx expr-
    #:stop-ids (map first G)
    #:bad-output (raise-syntax-error #f "expected a typed expression" expr)))

(define-base-type Number)
(define-type-constructor Arrow [in out])
(begin-for-syntax
  ;; Type Type -> Boolean
  (define (type-matches? A B) (equal? A B))
  ;; Stx Type Type -> Nothing
  (define (raise-type-mismatch e actual expected)
    (raise-syntax-error #f
      (format "type mismatch\n  expected: ~v\n  given:    ~v"
              expected actual))))

(define-typed-syntax #%datum
  [tc-e
   [G ⊢e #'(_ . n:number)]
   (er tc-e ≫ #'(quote n) ≫ (Number))])

(define-typed-syntax #%app
  [tc-e
   [G ⊢e #'(_ f:expr a:expr)]
   (ec G ⊢e #'f ≫ #'f- ⇒ (Arrow Ae B))
   (ec G ⊢e #'a ≫ #'a- ⇒ Aa)
   (unless (type-matches? Aa Ae)
     (raise-type-mismatch #'a Aa Ae))
   (er tc-e ≫ #'(rkt:#%app f- a-) ⇒ B)])

;; Now, decide what the environment representation will be.
;; The simplest I can think of is an association list between
;; identifiers and types:

(begin-for-syntax
  ;; An Env is a [Listof [List Id Type]]
  ;; Env Id -> Type
  (define (lookup G x)
    (second (assoc x G free-identifier=?)))
  ;; Env Id Type -> Env
  (define (extend G x type)
    (cons (list x type) G)))

;; Then our Var rule can use `lookup` and our Abstraction rule
;; can use `extend`.

(define-typed-syntax #%var
  [tc-e
   [G ⊢e #'(_ x:id)]
   (define type (lookup G x))
   (er tc-e ≫ x ⇒ type)])

(define-typed-syntax λ
  #:datum-literals [:]
  [tc-e
   [G ⊢e #'(_ ([x:id : τ:expr]) body:expr)]
   (define A (expand-type #'τ))
   (ec (extend G #'x A) ⊢ #'body ≫ #'body- ⇒ B)
   (er tc-e ≫ #'(rkt:λ (x) body-) ⇒ (Arrow A B))])

