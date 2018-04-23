#lang scribble/manual

@(require (for-label racket/base
                     syntax/parse
                     macrotypes-nonstx/expand-check-sugar
                     ))

@title{macrotypes-nonstx}

@defmodule[macrotypes-nonstx/expand-check-sugar]

@defform[#:literals [->]
         (define-expand-check-relation name
           [in-parameter ... -> out-parameter ...]
           call-syntax
           input-syntax
           output-syntax
           #:in-stx in-parameter
           #:out-stx out-parameter
           #:stop-ids stop-ids-expr
           #:bad-output bad-output-expr)
         #:grammar
         ([call-syntax [call-elem ...]]
          [input-syntax [input-elem ...]]
          [output-syntax [output-elem ...]]
          [call-elem in-parameter
                     out-parameter
                     literal-id]
          [input-elem in-parameter
                      literal-id]
          [output-elem out-parameter
                       literal-id])
         ]{
Defines @racket[name] as an @deftech{expand-check relation}, which can be
thought of as a type-check relation that checks a form by expanding
it.

To expand-check a form using it, use:

@racketblock[(ec call-elem ...)]

To define a macro that can be expand-checked by it, use:

@racketblock[
(define-typed-syntax macro-name
  ....
  [name
   [input-elem ...]
   premise ...
   (er name output-elem ...)]
  ....)]
}

@defform[(ec call-elem ...)
         #:grammar
         ([call-elem in-argument-expr
                     out-argument-pat
                     literal-id])]{
@racket[ec] stands for "expand-check".
                                   
Calls the @tech{expand-check relation} defined by the
@racket[call-elem]s. The @racket[in-argument-expr]s are expression
positions while the @racket[out-argument-pat]s are match patterns.
}

@defform[(er ec-name output-elem ...)
         #:grammar
         ([output-elem out-argument-expr
                       literal-id])]{
@racket[er] stands for "expansion-result".

Combines the outputs of the @tech{expand-check relation}
@racket[ec-name] into one value that can be returned from a macro.
This is typically used as the result of a macro that uses
@racket[define-typed-syntax]:

@racketblock[
(define-typed-syntax macro-name
  ....
  [ec-name
   [input-elem ...]
   premise ...
   (er ec-name output-elem ...)]
  ....)]
}

@defform[(define-typed-syntax name clause ...)
         #:grammar
         ([clause [ec-name
                   [input-elem ...]
                   premise ...
                   (er ec-name output-elem ...)]])]{
Defines @racket[name] as a macro that can be typechecked by any of the
@racket[ec-name] @tech{expand-check relation}s from the clauses.
}

