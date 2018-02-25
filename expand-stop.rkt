#lang racket/base

(provide expand/stop
         (for-template stop/error
                       stop/continue))

(module stop racket/base
  (provide stop/error stop/continue)
  (require (for-syntax racket/base))
  (define-syntax stop/error #f)
  (define-syntax-rule (stop/continue form) form))

(require (for-template 'stop))

(define (expand/stop stx ctx [stop-list '()])
  (local-expand stx ctx (list* #'stop/error #'stop/continue stop-list)))

