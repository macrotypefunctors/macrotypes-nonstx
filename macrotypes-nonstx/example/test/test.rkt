#lang s-exp "../example-lang.rkt"

(require "../testing.rkt")

(check-type 5 ⇒ Int)
(check-type (add1 6) ⇒ Int)
(check-type (add1 (add1 7)) ⇒ Int)
(check-type ((λ ([x : Int]) (add1 x)) 9) ⇒ Int)
(check-type ((λ ([f : (-> Int Int)])
               (f 10))
             (λ (x) (add1 x)))
            ⇒ Int)

(check-fail (add1 #true))