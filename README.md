gtp-benchmarks
===
[![Build Status](https://travis-ci.org/bennn/gtp-benchmarks.svg)](https://travis-ci.org/bennn/gtp-benchmarks)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/gtp-benchmarks/index.html)

Gradual Typing Performance benchmark programs.

This is a collection of Racket programs.
Each program can run in exponentially-many _configurations_ that differ in
 terms of their type annotations.


Overview
---

The benchmarks are in the `benchmarks/` folder.
Each benchmark is made of 2-4 folders:

- `untyped/` a Racket version of the benchmark (the untyped configuration)
- `typed/` a Typed Racket version of the benchmark (the typed configuration)
- (optional) `both/` extra Racket / Typed Racket files
- (optional) `base/` extra libraries or data files


To Run
---

The quick way:

- Go to the benchmark's directory
- Create a new directory
- Copy in all the `both/` files, if any
- Copy in your choice of `typed/` and `untyped/` files
- Run `main.rkt`


The correct way:

- Install the `gtp-measure` package (`raco pkg install gtp-measure`)
- Run `raco gtp-measure <PATH-TO-BENCHMARK>` (or run `raco gtp-measure --help`)
- Follow its instructions to get the output


The semi-automatic way:

- Run `racket make-lattice.rkt <PATH-TO-BENCHMARK>`, this creates a directory
  with all typed/untyped configurations of the benchmark.
- Go to one of the new directories, run `main.rkt`


Guidelines
---

The benchmarks try to meet the following "rock bottom" guidelines for giving
 reproducible performance data in reasonable time:

1. No I/O actions during timed computation
2. Able to run all typed/untyped configurations
3. Run for 1-5 seconds when untyped or fully-typed
4. Run for < 300 seconds in the worst case

Points 3 and 4 are in conflict.
For `forth` in particular, the untyped configuration runs extremely quickly
 but some partially-typed configurations take close to 5 minutes.


Dependencies
---

- `require-typed-check`



History
---

Original development: <https://github.com/nuprl/gradual-typing-performance>

Subsets / earlier-versions of these benchmarks have appeared in:

- [_Is Sound Gradual Typing Dead?_](https://dl.acm.org/citation.cfm?id=2837630). Asumu Takikawa, Daniel Feltey, Ben Greenman, Max S. New, Jan Vitek, and Matthias Felleisen. POPL 2016.
- _How to Evaluate the Performance of Gradual Type Systems_. Ben Greenman, Asumu Takikawa, Max S. New, Daniel Feltey, Robert Bruce Findler, Jan Vitek, and Matthias Felleisen. Submitted for publication.
- [_Sound Gradual Typing: Only Mostly Dead_](https://dl.acm.org/citation.cfm?id=3133878). Spenser Bauman, Carl Friedrich Bolz-Tereick, Jeremy Siek, and Sam Tobin-Hochstadt. OOPSLA 2017.
