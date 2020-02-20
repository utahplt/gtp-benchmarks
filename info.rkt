#lang info
(define collection "gtp-benchmarks")
(define deps '("base" "typed-racket-lib" "typed-racket-more" "require-typed-check"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "typed-racket-doc" "at-exp-lib" "gtp-util" "pict-lib" "scribble-abbrevs" "syntax-sloc" "with-cache"))
(define pkg-desc "Gradual typing benchmark programs")
(define version "5.2")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/gtp-benchmarks.scrbl" () ("Benchmarks"))))
(define compile-omit-paths '("benchmarks"))
(define test-omit-paths '("benchmarks"))

