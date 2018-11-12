#lang racket/base

(require racket/contract)
(provide
  chaperones-count/c
  racket-bin/count-chaperones?
  (contract-out
    [count-chaperones
      (-> racket-bin/count-chaperones? path-string? chaperones-count/c)]))

(require
  gtp-benchmarks/utilities/process-helper
  (only-in racket/math natural?)
  (only-in racket/port with-input-from-string open-output-nowhere)
  (only-in racket/list last)
  (only-in racket/system system)
  (only-in racket/path path-only)
  (only-in ffi/unsafe _array in-array _int get-ffi-obj))

;; =============================================================================

(define racket-name "racket")
(define raco-name "raco")
(define cc-name "count-chaperones")

(define (racket-bin? x)
  (and (path-string? x)
       (directory-exists? x)
       (file-exists? (build-path x racket-name))
       (file-exists? (build-path x raco-name))))

(define (has-count-chaperones-support? x)
  (define rkt (build-path x racket-name))
  (define tmp-filename (get-tmp-filename))
  (dynamic-wind
    (lambda ()
      (with-output-to-file tmp-filename #:exists 'replace
        (lambda ()
          (displayln "#lang typed/racket/base")))
      (append-get-chaperones-count! tmp-filename))
    (lambda ()
      (define cmd (format "~a ~a" (path->string rkt) tmp-filename))
      (define v*
        (parameterize ((current-output-port (open-output-nowhere))
                       (current-error-port (open-output-nowhere)))
          (process/error-port-filter cmd cc-log? read-cc-info)))
      (and (not (null? v*))
           (null? (cdr v*))
           (chaperones-count/c (car v*))
           #true))
    (lambda () (delete-file tmp-filename))))

(define (get-tmp-filename)
  (define tmp (find-system-path 'temp-dir))
  (for/or ((i (in-naturals)))
    (define fn (format "~a-~a.rkt" cc-name i))
    (define p (build-path tmp fn))
    (and (not (file-exists? p)) p)))

(define (racket-bin/count-chaperones? x)
  (and (racket-bin? x)
       (has-count-chaperones-support? x)))

(define chaperones-count-key* '(
  proc_makes
  proc_apps
  proc_wraps
  proc_maxdepth
  proc_depth*

  struct_makes
  struct_apps
  struct_wraps
  struct_maxdepth
  struct_depth*

  vec_makes
  vec_apps
  vec_wraps
  vec_maxdepth
  vec_depth*

  ;; from `vector-set-performance-stats!`
  current-process-milliseconds
  current-milliseconds
  current-gc-milliseconds
  num-garbage-collections ;; since startup, in the current place
  num-thread-context-switches ;; since startup
  num-internal-stack-overflows ;; since startup
  current-num-threads ;; threads that are not running, not suspended, and not unscheduled due to a synchronization
  num-syntax-objects ;; read from compiled code since startup
  num-hash-table-searches ;; note: this is a fixnum, and can overflow to the most negative fixnum
  num-additional-hash-slots ;; searched to complete hash searches, using double hashing
  num-machine-code-bytes ;; not reported by `current-memory-use`
  peak-allocated-bytes ;; just before a garbage collection
))

(define (chaperones-count-key? x)
  (and (symbol? x)
       (memq x chaperones-count-key*)
       #true))

(define chaperones-count/c
  (hash/c chaperones-count-key?
          (or/c natural? hash?)
          #:immutable #true
          #:flat? #true))

(define (append-get-chaperones-count! fn)
  (unless (file-exists? fn)
    (raise-argument-error 'append-get-chaperones-count! "file-exists?" fn))
  (with-output-to-file fn #:exists 'append
    (lambda ()
      (displayln "(require/typed (submod gtp-benchmarks/utilities/count-chaperones get-chaperones-count) (get-chaperones-count (-> Any)))")
      (displayln "(eprintf \"count-chaperones: ~s~n\" (get-chaperones-count))")
      (void))))

(define (count-chaperones bin-dir src)
  (append-get-chaperones-count! src)
  (define-values [base name _dir] (split-path src))
  (parameterize ([current-directory (if (path? base) base (current-directory))])
    (define cmd
      (let* ([raco-str (path->string (build-path bin-dir raco-name))]
             [racket-str (path->string (build-path bin-dir racket-name))]
             [name-str (path->string name)]
             [disable-rtc "DISABLE_REQUIRE_TYPED_CHECK=1"]
             [compile-cmd (format "~a ~a make ~a" disable-rtc raco-str name-str)]
             [run-cmd (format "~a ~a ~a" disable-rtc racket-str name-str)])
        (string-append compile-cmd " && " run-cmd)))
    (define v*
      (process/error-port-filter cmd cc-log? read-cc-info))
    (when (null? v*)
      (error 'count-chaperones "internal error: failed to get chaperone counts when executing file '~a'" src))
    (when (not (null? (cdr v*)))
      (eprintf "WARNING: got multiple chaperone counts from file '~a', returning the last result~n  All results: ~s~n" src v*))
    (last v*)))

(define cc-log?
  (let ((rx (regexp (string-append "^" cc-name ": "))))
    (lambda (ln)
      (regexp-match? rx ln))))

(define read-cc-info
  (let ([num-skip (+ 2 (string-length cc-name))])
    (lambda (ln)
      (define v (with-input-from-string (substring ln num-skip) read))
      (if (chaperones-count/c v)
        v
        (raise-arguments-error 'read-cc-info "expected string containing 'chaperones-count/c' hash" "string" ln)))))

;; =============================================================================

(module* get-chaperones-count racket/base
  (provide get-chaperones-count)
  (require
    (only-in ffi/unsafe _array in-array _int get-ffi-obj))
  (define v (make-vector 12 0))
  (define (get-chaperones-count)
    (void (vector-set-performance-stats! v))
    ;; TODO use a macro to make this "hash-filling" code?
    (make-immutable-hash
      (list
        (cons 'proc_makes (get-ffi-obj 'proc_makes #f _int))
        (cons 'proc_apps (get-ffi-obj 'proc_apps #f _int))
        (cons 'proc_wraps (get-ffi-obj 'proc_wraps #f _int))
        (cons 'proc_maxdepth (get-ffi-obj 'proc_maxdepth #f _int))
        (cons 'proc_depth* (for/hash ([v (in-array (get-ffi-obj 'proc_depth #f (_array _int 901)))]
                                      [k (in-naturals)]
                                      #:when (not (zero? v)))
                             (values k v)))
        (cons 'struct_makes (get-ffi-obj 'struct_makes #f _int))
        (cons 'struct_apps (get-ffi-obj 'struct_apps #f _int))
        (cons 'struct_wraps (get-ffi-obj 'struct_wraps #f _int))
        (cons 'struct_maxdepth (get-ffi-obj 'struct_maxdepth #f _int))
        (cons 'struct_depth* (for/hash ([v (in-array (get-ffi-obj 'struct_depth #f (_array _int 901)))]
                                        [k (in-naturals)]
                                        #:when (not (zero? v)))
                               (values k v)))
        (cons 'vec_makes (get-ffi-obj 'vec_makes #f _int))
        (cons 'vec_apps (get-ffi-obj 'vec_apps #f _int))
        (cons 'vec_wraps (get-ffi-obj 'vec_wraps #f _int))
        (cons 'vec_maxdepth (get-ffi-obj 'vec_maxdepth #f _int))
        (cons 'vec_depth* (for/hash ([v (in-array (get-ffi-obj 'vec_depth #f (_array _int 901)))]
                                     [k (in-naturals)]
                                     #:when (not (zero? v)))
                            (values k v)))
        (cons 'current-process-milliseconds (vector-ref v 0))
        (cons 'current-milliseconds (vector-ref v 1))
        (cons 'current-gc-milliseconds (vector-ref v 2))
        (cons 'num-garbage-collections (vector-ref v 3))
        (cons 'num-thread-context-switches (vector-ref v 4))
        (cons 'num-internal-stack-overflows (vector-ref v 5))
        (cons 'current-num-threads (vector-ref v 6))
        (cons 'num-syntax-objects (vector-ref v 7))
        (cons 'num-hash-table-searches (vector-ref v 8))
        (cons 'num-additional-hash-slots (vector-ref v 9))
        (cons 'num-machine-code-bytes (vector-ref v 10))
        (cons 'peak-allocated-bytes (vector-ref v 11))))))


;; =============================================================================

(module+ test
  (require rackunit)

  (define SAMPLE-MESSAGE "count-chaperones: #hash()")

  (define CI? (and (getenv "CI") #true))

  (test-case "racket-bin?"
    (check-false (racket-bin? "yolo"))
    (check-false (racket-bin? 42))
    (unless CI?
      (check-true (racket-bin? (path-only (find-executable-path (find-system-path 'exec-file)))))))

  (test-case "has-count-chaperones-support?"
    (define my-fork "/Users/ben/code/racket/fork/racket/bin")
    (define my-6.12c "/Users/ben/code/racket/6.12c/racket/bin")
    (when (and (not CI?) (directory-exists? my-fork) (directory-exists? my-6.12c))
      (check-false (has-count-chaperones-support? my-fork))
      (check-false (racket-bin/count-chaperones? my-fork))
      (check-true (has-count-chaperones-support? my-6.12c))
      (check-true (racket-bin/count-chaperones? my-6.12c))))

  (test-case "get-tmp-filename"
    (define x (get-tmp-filename))
    (check-true (path-string? x))
    (check-false (file-exists? x))
    (check-equal? (path-only x) (find-system-path 'temp-dir)))

  (test-case "chaperones-count-key?"
    (check-true (chaperones-count-key? 'proc_makes))
    (check-false (chaperones-count-key? 'prolo))
    (check-false (chaperones-count-key? 44)))

  (test-case "chaperones-count/c"
    (check-true (chaperones-count/c #hash()))
    (check-true (chaperones-count/c '#hash((proc_apps . 0) (proc_makes . 1) (struct_depth* . #hash((0 . 10))))))
    (check-false (chaperones-count/c '#hash((proc_wins . A))))
    (check-false (chaperones-count/c #hash((0 . 0))))
    (check-false (chaperones-count/c 4)))

  (test-case "cc-log?"
    (check-true (cc-log? SAMPLE-MESSAGE))
    (check-false (cc-log? (substring SAMPLE-MESSAGE 2)))
    (check-false (cc-log? "")))

  (test-case "read-cc-info"
    (define v (read-cc-info SAMPLE-MESSAGE))
    (check-pred hash? v))

)
