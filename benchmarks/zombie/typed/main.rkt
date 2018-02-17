#lang typed/racket/base

(require
  require-typed-check
  "image-adapted.rkt"
)
(require/typed/check "zombie.rkt"
  (w0 World)
  (world-on-mouse (-> World (-> Real Real String World)))
  (world-on-tick (-> World (-> World)))
)

(define-type World
  (-> Symbol (U (Pairof 'on-mouse (-> Real Real String World))
                (Pairof 'on-tick (-> World))
                (Pairof 'to-draw (-> Image))
                (Pairof 'stop-when (-> Boolean)))))

;; =============================================================================

(: replay (-> World (Listof Any) Void))
(define (replay w0 hist)
 (let loop ((w : World w0)
            (h : (Listof Any) hist))
  (cond
   [(null? h)
    (void)]
   [(not (list? (car h)))
    (error "input error")]
   [else
    (define m (caar h))
    (define as (cdar h))
    (case m
     [(on-mouse)
      (define r (apply (world-on-mouse w) (cast as (List Real Real String))))
      (loop r (cdr h))]
     [(on-tick)
      (define r ((world-on-tick w)))
      (loop r (cdr h))])])))

(define TEST
  (with-input-from-file "../base/zombie-hist.rktd" read))

(: main (-> Any Void))
(define (main hist)
  (cond
   [(list? hist)
    (for ([i : Integer (in-range 100)])
      (replay w0 hist))]
   [else
    (error "bad input")]))

(time (main TEST))
