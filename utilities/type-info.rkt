#lang racket/base

;; Tools for collecting type annotations

(provide
  compile/require-typed-check-info
  require-typed-check-info-src
  require-typed-check-info-sexp)

(require
  require-typed-check/logging
  gtp-benchmarks/utilities/process-helper
  (only-in racket/port
    with-input-from-string)
  (only-in racket/path
    file-name-from-path
    path-only))

;; =============================================================================

(define require-typed-check "require-typed-check")

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
    (define cmd
      (let ((raco-str (find-raco)))
        (format "PLTSTDERR='error info@require-typed-check' ~a make ~a" raco-str (path->string name))))
    (process/error-port-filter cmd rtc-log? read-rtc-info)))

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
