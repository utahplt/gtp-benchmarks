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
