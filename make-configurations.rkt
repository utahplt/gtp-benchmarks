#lang racket/base
(require
  (only-in file/glob glob in-glob)
  (only-in racket/format ~r)
  (only-in racket/file copy-directory/files)
  (only-in racket/path file-name-from-path))

(define (natural->bitstring n #:bits pad-width)
  (~r n #:base 2 #:min-width pad-width #:pad-string "0"))

(define (make-configurations dir)
  (define config-dir (format "~a-configurations" (path->string (file-name-from-path dir))))
  (printf "Making directory '~a' ...~n" config-dir)
  (make-directory config-dir)
  (define base-dir (build-path dir "base"))
  (define both-dir (build-path dir "both"))
  (when (directory-exists? base-dir)
    (copy-directory/files base-dir (build-path config-dir "base")))
  (define f*
    (let ([u* (for/list ((x (in-glob (build-path dir "untyped" "*.rkt")))) (path->string (file-name-from-path x)))]
          [t* (for/list ((x (in-glob (build-path dir "typed" "*.rkt")))) (path->string (file-name-from-path x)))])
      (if (equal? u* t*)
        u*
        (raise-argument-error 'make-configurations "typed and untyped folders have different .rkt files" "typed" t* "untyped" u*))))
  (define num-modules (length f*))
  (define num-configs (expt num-modules 2))
  (for ((i (in-range num-configs)))
    (define cfg (natural->bitstring i #:bits num-modules))
    (define cfg-dir (format "configuration~a" cfg))
    (make-directory (build-path config-dir cfg-dir))
    (when (directory-exists? both-dir)
      (for ((f (in-glob (build-path both-dir "*"))))
        (copy-file f (build-path config-dir cfg-dir (file-name-from-path f)))))
    (for ((c (in-string cfg))
          (f (in-list f*)))
      (if (eq? c #\0)
        (copy-file (build-path dir "untyped" f) (build-path config-dir cfg-dir f))
        (copy-file (build-path dir "typed" f) (build-path config-dir cfg-dir f))))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "make-configurations"
    #:args (dir)
    (make-configurations dir)))
