#lang racket/base

(require racket/contract racket/path file/glob)
(provide (contract-out (copy-configuration (-> bitstring? void?))))

(define (copy-configuration bits)
  (ensure-dir "config")
  (void
    (for ((b-f (in-glob (build-path "both" "*.rkt"))))
      (copy-file b-f (build-path "config" (file-name-from-path b-f)) #t)))
  (define-values [t* u*] (typed/untyped-files "."))
  (for ([c (in-string bits)]
        [u (in-list u*)]
        [t (in-list t*)])
    (case c
    [(#\0) (copy-file u (build-path "config" (file-name-from-path u)) #t)]
    [(#\1) (copy-file t (build-path "config" (file-name-from-path t))#t)]
    [else (error 'copy-configuration "die")])))

(define (ensure-dir dir)
  (unless (directory-exists? dir)
    (make-directory dir)))

(define (bitstring? str)
(for/and ((c (in-string str)))
(memq c '(#\0 #\1))))

(define (typed/untyped-files dir)
  (define u-g (sort (glob (build-path dir "untyped" "*.rkt")) string<? #:key short-name))
  (define t-g (sort (glob (build-path dir "typed" "*.rkt")) string<? #:key short-name))
  (define u-n (map short-name u-g))
  (define t-n (map short-name t-g))
  (unless (equal? u-n t-n)
    (raise-arguments-error 'typed/untyped-files "mismatch" "typed" t-n "untyped" u-n))
  (values t-g u-g))

(define (short-name path)
(path->string (file-name-from-path path)))

(module* main racket/base
  (require (submod "..") racket/cmdline)
  (command-line
  #:program "copy-configuration"
  #:args (cfg)
  (copy-configuration cfg)))
