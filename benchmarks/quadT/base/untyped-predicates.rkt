#lang racket/base

(provide
  Breakpoint?
  Font-Name?
  Font-Size?
  Font-Style?
  Font-Weight?
  QuadAttrKey?
  QuadAttrValue?
  QuadAttrs?
  quad-attrs?
  quad?
  block-break?
  column-break?
  column?
  line?
  optical-kern?
  page-break?
  run?
  spacer?
  word-break?
  word?)

(require
  racket/math
  racket/contract
  (only-in "untyped.rkt" index?)
  (only-in racket/draw font-style/c font-weight/c))

;; -----------------------------------------------------------------------------
;; "../base/core-types.rkt"

(define Breakpoint? nonnegative-integer?)
(define Font-Name? string?)
(define (Font-Size? x) (and (flonum? x) (positive? x)))
(define Font-Style? (contract-first-order font-style/c))
(define Font-Weight? (contract-first-order font-weight/c))
(define QuadAttrKey? symbol?)
(define QuadName? symbol?)

(define (QuadAttrValue? x) (or (flonum? x) (index? x) (string? x) (symbol? x) (boolean? x) (quad? x) (QuadAttrs? x) (QuadList? x) (exact-integer? x)))
(define (QuadAttr? x)
  ;;(contract-first-order-passes? (cons/c QuadAttrKey? QuadAttrValue?) x)
  (and (pair? x)
       (QuadAttrKey? (car x))
       (QuadAttrValue? (cdr x))))

(define QuadAttrs? (listof QuadAttr?))
(define (QuadListItem? x)
  ;;(contract-first-order-passes? (or/c string? quad?) x)
  (or (string? x)
      (quad? x)))

(define QuadList? (listof QuadListItem?))
(define (quad? x)
  ;; (list*/c QuadName? QuadAttrs? QuadList?)
  (and (pair? x)
       (let ((a (car x))
             (b (cdr x)))
         (and (QuadName? a)
              (pair? b)
              (let ((aa (car b))
                    (bb (cdr b)))
                (and (QuadAttrs? aa)
                     (QuadList? bb)))))))

(define quad-attrs? QuadAttrs?)
(define GroupQuadListItem? quad?)
(define GroupQuadList? (listof GroupQuadListItem?))

;; -----------------------------------------------------------------------------
;; "../base/quad-types.rkt"

(define (make-quad-list-ctc sym)
  (make-?-list-ctc sym QuadList?))

(define (make-group-list-ctc sym)
  (make-?-list-ctc sym GroupQuadList?))

(define ((make-?-list-ctc sym QL?) x)
  (and (pair? x)
       (let ((a (car x))
             (b (cdr x)))
         (and (symbol? a)
              (eq? a sym)
              (pair? b)
              (let ((aa (car b))
                    (bb (cdr b)))
                (and (QuadAttrs? aa)
                     (QL? bb)))))))

(define block-break?  (make-quad-list-ctc 'block-break))
(define column-break?  (make-quad-list-ctc 'column-break))
(define column? (make-group-list-ctc 'column))
(define line? (make-group-list-ctc 'line))
(define optical-kern? (make-quad-list-ctc 'optical-kern))
(define page-break? (make-quad-list-ctc 'page-break))
(define run? (make-quad-list-ctc 'run))
(define spacer? (make-quad-list-ctc 'spacer))
(define word-break? (make-quad-list-ctc 'word-break))
(define word? (make-quad-list-ctc 'word))
