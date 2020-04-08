#lang gtp-measure/manifest

;; Sample configuration file to run the `gtp-measure` package on the full
;; `gtp-benchmarks` suite.
;;
;; WARNING: this file may be out of date; check that all benchmarks are here!
;;
;; Modify the TODO items below with absolute paths for your machine.

#:config #hash(
  (bin . "/TODO/racket/bin/")
  ;; use the `racket` and `raco` binaries here to compile & run

  (entry-point . "main.rkt")
  ;; run this file for each configuration

  (jit-warmup . 1)
  ;; run each configuration this many times before collecting measurements

  (iterations . 8)
  ;; collect this many running times

  (cutoff . 20)
  ;; use random sampling for benchmarks with at least this many modules

  (num-samples . 10)
  ;; for sampling, repeat this many times

  (sample-factor . 10)
  ;; for sampling, use (* sample-factor num-modules) configs per group
)

("/TODO/gtp-benchmarks/benchmarks/acquire" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/dungeon" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/forth" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/fsm" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/fsmoo" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/gregor" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/jpeg" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/kcfa" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/lnm" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/mbta" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/morsecode" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/quadT" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/quadU" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/sieve" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/snake" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/suffixtree" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/synth" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/take5" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/tetris" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/zombie" . typed-untyped)
("/TODO/gtp-benchmarks/benchmarks/zordoz" . typed-untyped)
