#lang racket/base

(require racket/contract)
(provide
  (contract-out
    [process/error-port-filter
      (-> string?
          (-> string? any/c)
          (-> string? any/c)
          (listof string?))]))

(require
  (only-in racket/system
    process))

;; =============================================================================

(define (log-info str . arg*)
  (apply printf (string-append "INFO: " str "~n") arg*))

(define (log-error str . arg*)
  (apply eprintf (string-append "ERROR: " str "~n") arg*))

(define (process/error-port-filter cmd relevant-log? read-log)
  (define-values [pout pin pid perr pc]
    (apply values (process cmd)))
  (define v*
    (let loop ()
      (define pstatus (pc 'status))
      (case pstatus
        ((running)
         (log-info "... running")
         (pc 'wait)
         (loop))
        ((done-error)
         (log-error "failed to execute command '~a'" cmd)
         (for ((ln (in-lines perr)))
           (log-error ln))
         '())
        ((done-ok)
         (log-info "... done-ok")
         (for/list ((ln (in-lines perr))
                    #:when (relevant-log? ln))
           (read-log ln)))
        (else
          (loop)))))
    (close-output-port pin)
    (close-input-port perr)
    (close-input-port pout)
    v*)
