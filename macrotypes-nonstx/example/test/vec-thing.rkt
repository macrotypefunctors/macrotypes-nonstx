#lang s-exp "../vec-thing.rkt"

(require "../testing.rkt")

(check-type 5                          ⇒ (Fin 6))
(check-type (make-vec 6 7)             ⇒ (Vec 6 (Fin 8)))
(check-type (vec-ref (make-vec 6 7) 5) ⇒ (Fin 8))
