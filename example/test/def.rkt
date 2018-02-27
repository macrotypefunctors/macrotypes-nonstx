#lang s-exp "../definition-lang.rkt"

;; basic
(define a : Int 5)
(define b : Int (add1 6))
(define c : Int (add1 (add1 7)))
(define d : Int ((λ ([x : Int]) (add1 x)) 9))
(define e : Int
  ((λ ([f : (-> Int Int)])
     (f 10))
   (λ (x) (add1 x))))

;; references to previous vars
(define use-a : Int a)
(define use-abc : Int
  ((((λ ([x : Int]) (λ ([y : Int]) (λ ([z : Int]) x)))
     a)
    b)
   c))

;; self-recursive references
(define loop-f : (-> Int Int)
  (λ (x) (loop-f (add1 x))))

;; multually recursive references
(define mutual-f : (-> Int Int)
  (λ (x) (mutual-g (add1 x))))

(define mutual-g : (-> Int Int)
  (λ (x) (mutual-f (add1 x))))

