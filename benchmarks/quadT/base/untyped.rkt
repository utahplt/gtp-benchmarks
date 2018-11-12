#lang racket/base

(provide
  quad
  any?
  hashtabletop?
  assert
  float?
  index?
  vectorof
  listof
  nonnegative-float?
  hash/c
  index-type?
  entry-type?
  value-type?
  entry->value-type/c)

(require
  racket/contract
  racket/math
  (only-in racket/unsafe/ops unsafe-fx>=))

(define (index? x)
  (and (fixnum? x) (unsafe-fx>= x 0) (fixnum? (* x 4))))

(define (assert x p)
  (contract p x '+ '-))

;; copied from 'core-types.rkt'
(define-syntax-rule (quad name attrs items)
  (list* name attrs items))

(define hashtabletop? hash?)

(define any? any/c)

(define float? flonum?)

(define (nonnegative-float? x)
  (and (flonum? x) (<= 0  x)))

;; -----------------------------------------------------------------------------

(define index-type? nonnegative-integer?)
(define entry-type? any?)
(define value-type? flonum?)
(define entry->value-type/c
  (-> entry-type? value-type?))
