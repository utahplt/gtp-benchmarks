#lang racket/base

(provide
  (struct-out $ocm)
  set-$ocm-tentative!
  set-$ocm-min-entrys!
  set-$ocm-min-row-indices!
  set-$ocm-finished!
  set-$ocm-base!
  entry-type?
  index-type?
  entry->value-type/c
)

(require
  "../base/untyped.rkt"
  (only-in racket/contract ->)
  racket/math)

;; =============================================================================

(struct $ocm
  (min-entrys
   min-row-indices
   finished
   matrix-proc
   entry->value
   base
   tentative)
#:transparent #:mutable)

;; -----------------------------------------------------------------------------

(define index-type? nonnegative-integer?)
(define entry-type? any?)
(define value-type? flonum?)
(define entry->value-type/c
  (-> entry-type? value-type?))
