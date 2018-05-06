#lang racket/base

;; Tools for collecting type annotations

(provide
  compile/require-typed-check-info
  require-typed-check-info-src
  require-typed-check-info-sexp)

(require
  require-typed-check/logging
  (only-in racket/system
    process)
  (only-in racket/port
    with-input-from-string)
  (only-in racket/path
    file-name-from-path
    path-only))

;; =============================================================================

(define require-typed-check "require-typed-check")

(define (log-info str . arg*)
  (apply printf (string-append "INFO: " str "~n") arg*))

(define (log-error str . arg*)
  (apply eprintf (string-append "ERROR: " str "~n") arg*))

(define (find-raco)
  (define sp
    (find-executable-path (find-system-path 'exec-file)))
  (unless sp
    (raise-arguments-error 'find-raco "failed to `(find-system-path 'exec-file)`" "result" sp))
  (define parent
    (or (path-only sp) (current-directory)))
  (define raco-path
    (build-path parent "raco"))
  (unless (file-exists? raco-path)
    (raise-arguments-error 'find-raco "failed to find 'raco' executable" "path" raco-path))
  (path->string raco-path))

(define (compile/require-typed-check-info src)
  (define-values [base name _dir] (split-path src))
  (parameterize ([current-directory (if (path? base) base (current-directory))])
    (define-values [pout pin pid perr pc]
      (let* ([raco-str (find-raco)]
             [cmd (format "PLTSTDERR='error info@require-typed-check' ~a make ~a" raco-str (path->string name))])
        (apply values (process cmd))))
    (define v*
      (let loop ()
        (define pstatus (pc 'status))
        (case pstatus
          ((running)
           (log-info "... running")
           (pc 'wait)
           (loop))
          ((done-error)
           (log-error "failed to compile '~a'" src)
           (for ((ln (in-lines perr)))
             (log-error ln))
           '())
          ((done-ok)
           (log-info "... done-ok")
           (for/list ((ln (in-lines perr))
                      #:when (rtc-log? ln))
             (read-rtc-info ln)))
          (else
            (loop)))))
      (close-output-port pin)
      (close-input-port perr)
      (close-input-port pout)
      v*))

(define rtc-log?
  (let ((rx (regexp (string-append "^" require-typed-check ": "))))
    (lambda (ln)
      (regexp-match? rx ln))))

(define read-rtc-info
  (let ([num-skip (+ 2 (string-length require-typed-check))])
    (lambda (ln)
      (define v (with-input-from-string (substring ln num-skip) read))
      (if (require-typed-check-info? v)
        v
        (raise-arguments-error 'read-rtc-info "expected string containing 'require-typed-check-info' struct" "string" ln)))))

(define (sexp->module-name rtc)
  (cadr rtc))

(define (sexp->clause* rtc)
  (cddr rtc))

;; =============================================================================

(module+ test
  (require rackunit)

  (define SAMPLE-MESSAGE "require-typed-check: #s(require-typed-check-info \"sieve/tmp/main.rkt\" (require/typed/check \"streams.rkt\" (#:struct stream ((first : Natural) (rest : (-> stream)))) (make-stream (-> Natural (-> stream) stream)) (stream-unfold (-> stream (values Natural stream))) (stream-get (-> stream Natural Natural)) (stream-take (-> stream Natural (Listof Natural)))))")

  (test-case "rtc-log?"
    (check-true (rtc-log? SAMPLE-MESSAGE))
    (check-false (rtc-log? (substring SAMPLE-MESSAGE 2)))
    (check-false (rtc-log? "")))

  (test-case "read-rtc-info"
    (define v (read-rtc-info SAMPLE-MESSAGE))
    (check-true (require-typed-check-info? v))
    (check-true (string? (require-typed-check-info-src v)))
    (check-true (pair? (require-typed-check-info-sexp v))))

  (test-case "sexp->module-name"
    (define rtc (read-rtc-info SAMPLE-MESSAGE))
    (define x (require-typed-check-info-sexp rtc))
    (define m (sexp->module-name x))
    (check-true (string? m))
    (check-equal? (path->string (file-name-from-path m)) "streams.rkt"))

  (test-case "sexp->clause*"
    (define rtc (read-rtc-info SAMPLE-MESSAGE))
    (define x (require-typed-check-info-sexp rtc))
    (define c* (sexp->clause* x))
    (check-true (list? c*))
    (check-equal? (length c*) 5)
    (check-equal? (car (car c*)) '#:struct))

)
