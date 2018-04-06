gtp-benchmarks
===

Gradual Typing Performance benchmark programs.


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


Rules
---

- No I/O actions during timed computation
- Able to run all typed/untyped configurations
- Run for 1-5 seconds when untyped or fully-typed
- Run for < 300 seconds in the worst case


Dependencies
---

- `require-typed-check`


Glossary
---

[POPL 2016] = _Is Sound Gradual Typing Dead?_


History
---

Original development: <https://github.com/nuprl/gradual-typing-performance>
