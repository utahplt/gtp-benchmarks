#lang scribble/manual

@require[
  gtp-benchmarks/scribblings/util
  scriblib/figure
  scribble/example
  (for-label
    require-typed-check
    racket/base
    (only-in typed/racket/base require/typed))]

@title{GTP Benchmarks}

@section[#:tag "gtp:descriptions"]{Benchmark Descriptions}

This section summarizes the benchmark programs.
The @emph{author} and @emph{source} fields credit the authors of the code that
 inspired the benchmarks.
The @emph{dependencies} field lists any libraries outside the core of Racket
 and Typed Racket that the benchmarks use; we assume that programmers who
 use gradual typing cannot change the language of these libraries (the libraries
 are either typed or untyped).

Each benchmark comes with a short description of its behavior,
 a module dependence graph,
 and the names of its @emph{migratable} and @emph{fixed} modules.
In the module graphs, edges point from one module to another whenever one
 module requires another.
Nodes for migratable modules are circles --- these are the modules we apply
 gradual typing to.
Nodes for fixed modules are squares --- these modules are the same in all
 configurations.

Note: these graphs do not show @emph{type adaptor modules}.

@format-benchmark[#:name 'acquire
                  #:author "Matthias Felleisen"
                  #:purpose "Game"
                  #:origin "https://github.com/mfelleisen/Acquire"
                  #:depends '()]{
  @; TODO ... what is the game, who are the players, how many rounds, what values at boundaries?
  Simulates a board game between player objects.
  The players send messages to an administrator object; the administrator
   enforces the rules of the game.
}
@format-benchmark[#:name 'dungeon
                  #:author "Vincent St-Amour"
                  #:purpose "Maze generator"
                  #:origin #f
                  #:depends '()]{
  Builds a maze of wall and floor objects by drawing first-class classes from
   a list.
}
@format-benchmark[#:name 'forth
                  #:author "Ben Greenman"
                  #:purpose "Interpreter"
                  #:origin "https://github.com/bennn/forth"
                  #:depends '()]{
  Interprets Forth programs.
  The interpreter represents calculator commands as a list of first-class objects.
}
@format-benchmark[#:name 'fsm
                  #:author "Linh Chi Nguyen"
                  #:purpose "Economy Simulator"
                  #:origin "https://github.com/ayaderaghul/sample-fsm"
                  #:depends '()]{
  @; TODO sentence too long
  Simulates an economy with finite-state automata.
  The economy is implemented as a vector; this vector repeatedly crosses between
  modules in the benchmark.
}
@format-benchmark[#:name 'gregor
                  #:author "Jon Zeppieri"
                  #:purpose "Date and time library"
                  #:origin "https://github.com/97jaz/gregor"
                  #:depends '((cldr untyped) (tzinfo untyped))]{
  Provides tools for manipulating calendar dates.
  The benchmark builds tens of date values and runs unit tests on these values.
}
@format-benchmark[#:name 'jpeg
                  #:author "Andy Wingo"
                  #:purpose "JPEG parsing and transformation"
                  #:origin "https://github.com/wingo/racket-jpeg"
                  #:depends '((math/array typed) (rnrs/bytevectors-6 untyped))]{
  Parses a bytestream of JPEG data to an internal representation, then
   serializes the result.
}
@format-benchmark[#:name 'kcfa
                  #:author "Matt Might"
                  #:purpose "Explanation of k-CFA"
                  #:origin "http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/"
                  #:depends '()]{
  Performs 2-CFA on a lambda calculus equation built from Church numerals;
  specifically, it analyzes an encoding of @tt{(2 * (1 + 3)) = (1 * 2)}.
}
@format-benchmark[#:name 'lnm
                  #:author "Ben Greenman"
                  #:purpose "Graphing"
                  #:origin "https://github.com/nuprl/gradual-typing-performance/tree/master/paper/popl-2016/scripts"
                  #:depends '((plot typed) (math/statistics typed))]{
  Renders a plot and spreadsheet for some gradual typing data.
  Two modules in this benchmark are tightly-coupled to Typed Racket libraries;
   typing both modules improves performance.
}
@format-benchmark[#:name 'mbta
                  #:author "Matthias Felleisen"
                  #:purpose "Interactive map"
                  #:origin #f
                  #:depends '((graph untyped))]{
  Builds a map of Boston's subway system and answers reachability queries.
  The map encapsulates a boundary to Racket's untyped @library{graph} library;
   when the map is typed, the boundary to @library{graph} is a
   performance bottleneck.
}
@format-benchmark[#:name 'morsecode
                  #:author '("John B. Clements" "Neil Van Dyke")
                  #:purpose "Morse code trainer"
                  #:origin "https://github.com/jbclements/morse-code-trainer/tree/master/morse-code-trainer"
                  #:depends '()]{
  Computes Levenshtein distances and morse code translations for a
  sequence of pairs of words.
}
@format-benchmark[#:name 'quadBG
                  #:author "Ben Greenman"
                  #:purpose "Typesetting"
                  #:origin "https://github.com/mbutterick/quad"
                  #:depends '((csp untyped))]{
  Converts S-expression source code to @tt{PDF} format.
  This benchmark is similar to the @bm{quadMB} benchmark except for its
  choice of type annotations; the types that @bm{quadBG} uses to describe its
  input correspond to flat contracts.
}
@format-benchmark[#:name 'quadMB
                  #:author "Matthew Butterick"
                  #:purpose "Typesetting"
                  #:origin "https://github.com/mbutterick/quad"
                  #:depends '((csp untyped))]{
  Converts S-expression source code to @tt{PDF} format (same as @bm{quadBG}).
  This benchmark performs slightly different computations in its typed and
  untyped versions: the typed version uses recursive predicates (derived from
  its types) to guide control flow, whereas the untyped version uses flat
  predicates.
}
@format-benchmark[#:name 'sieve
                  #:author "Ben Greenman"
                  #:purpose "Generate prime numbers"
                  #:origin "https://github.com/nuprl/gradual-typing-performance/tree/master/benchmarks/sieve"
                  #:depends '()]{
  Computes prime numbers using a stream library.
}
@format-benchmark[#:name 'snake
                  #:author "David Van Horn"
                  #:purpose "Game"
                  #:origin "https://github.com/philnguyen/soft-contract"
                  #:depends '()]{
  Functional program that implements the Snake game.
  The benchmark folds over a sequence of game moves.
}
@format-benchmark[#:name 'suffixtree
                  #:author "Danny Yoo"
                  #:purpose "Ukkonen's suffix tree algorithm"
                  #:origin "https://github.com/dyoo/suffixtree"
                  #:depends '()]{
  Computes longest common subsequences between strings.
}
@format-benchmark[#:name 'synth
                  #:author "Vincent St. Amour & Neil Toronto"
                  #:purpose "Music synthesizer"
                  #:origin "http://github.com/stamourv/synth"
                  #:depends '()]{
  Converts a description of notes and drum beats to @tt{WAV} format.
  This benchmark creates a large number of vectors (to represent notes)
  but rarely reads from the vectors.
}
@format-benchmark[#:name 'take5
                  #:author "Matthias Felleisen"
                  #:purpose "Game"
                  #:origin "https://github.com/mfelleisen/take5"
                  #:depends '()]{
  Simulates a card game between player objects.
  The players communicate through a dealer object.
}
@format-benchmark[#:name 'tetris
                  #:author "David Van Horn"
                  #:purpose "Game"
                  #:origin "https://github.com/philnguyen/soft-contract"
                  #:depends '()]{
  Functional implementation of Tetris; the benchmark replays a pre-recorded
  sequence of moves.
}
@format-benchmark[#:name 'zombie
                  #:author "David Van Horn"
                  #:purpose "Game"
                  #:origin "https://github.com/philnguyen/soft-contract"
                  #:depends '()]{
  Implements a game where players avoid enemies.
  This benchmark uses an encoding of objects as higher-order functions to implement
  the game player, the enemies, and the board.
}
@format-benchmark[#:name 'zordoz
                  #:author "Ben Greenman"
                  #:purpose "Bytecode explorer"
                  #:origin "http://github.com/bennn/zordoz"
                  #:depends '((compiler/zo-parse untyped) (compiler/zo-structs untyped))]{
  Traverses Racket bytecode (@tt{.zo} files).
  The @library{compiler} library defines the bytecode data structures.
}


@section{Static Benchmark Details}

@subsection{Benchmark Size}

@figure["fig:static-summary" @elem{Size of the benchmarks.}
  @tabulate-gradual-typing-benchmarks-size[]]

The table in @figure-ref{fig:static-summary} quantifies the benchmarks' size
and structure.
The @emph{Untyped LOC} column lists the number of non-whitespace, non-comment lines of
code in the untyped version of each benchmark (computed by the @hyperlink["https://github.com/AlexKnauth/syntax-sloc"]{@tt{syntax-sloc}} library).
The @emph{Annotation LOC} is the additional number of lines in the typed version
of each benchmark; this estimates the number of type annotations in the typed version.
The @emph{# Modules} column is the number of modules in each benchmark, and
lastly the @emph{# Bnd.} and @emph{# Exp.} columns summarize the dependencies between
these modules.
One boundary (counted in @emph{# Bnd.}) is one import statement from one module
in the benchmark to another.
One export (counted in @emph{# Exp.}) is one identifier provided by one module
in the benchmark.


@subsection{Benchmark Types}

This section contains the source code for each boundary between modules in
 the benchmarks.
The data format is:

@nested[#:style 'inset
  @list[@bold{benchmark name}
        @linebreak[]
        @emph{importing module}
        @linebreak[]
        @verbatim{'(require-typed-check exporting-module ...)}]]

In other words, the data below shows the @racket[require/typed/check] forms
 for each module in each benchmark.

Depending on the configuration, a @racket[require/typed/check] expands to
 either a @racket[require] or a @racket[require/typed] form.
The latter form compiles types to contracts; these contracts are the reason
 why some configurations run slower than others.
In this way, the types below give an idea of the kind of overhead each benchmark
 may suffer from.

Note: the data below may refer to type aliases.
See the source code for each benchmark to find what the aliases stand for.

@format-require-typed-check-info[]



@section{Dynamic Benchmark Details}
@; TODO (theoretical) worst-case performance [[contract profile]]
@; TODO `raco expand` output of worst-case versions (for the contracts)

This section reports low-level details about the execution of the
 @emph{theoretical worst-case configuration} (for short: TWC) of each benchmark.
The TWC configuration is the one in which every boundary between
 migratable modules is guarded by a contract.
In Typed Racket terms, this means every import between migratable modules is
 via @racket[require/typed].
(This configuration has worse performance than any configuration that
 combines typed and untyped modules --- because in those real configurations,
 only some of the boundaries are guarded with contracts.)

The data in this section was obtained by running a version of Racket v6.12
 instrumented with compiler-level counters to track chaperone use.
The patch implementing the counters is part of this repository's source code,
 and is adapted from a patch by @hyperlink["http://cs.brown.edu/~sstrickl/chaperones/"]{Strickland, Tobin-Hochstadt, Findler, and Flatt (2012)}.


@subsection{Time and Garbage Collection Details}

The data in @figure-ref{fig:dynamic-time} comes from calling @racket[vector-set-performance-stats!]
 after running the TWC configuration.
Column @emph{Milliseconds} column reports total running time (including setup, i.e., reading from data files) in milliseconds.
Column @emph{GC Milliseconds} column reports the total garbage collection time.
Column @emph{Num. GC} reports the number of garbage collections performed since start-up in the current place.
Column @emph{Peak Bytes} reports the largest number of bytes that were allocated just before a garbage collection.


@figure["fig:dynamic-time" @elem{TWC Time Details}
  @format-time-info[]]


@subsection{Chaperones Details}

The data in @figure-ref{fig:dynamic-chaperones} reports low-level details about chaperones.

Quick reference:

@itemlist[
@item{@emph{Proc.} = procedure chaperone}
@item{@emph{Struct} = struct chaperone}
@item{@emph{Vec} = vector chaperone}
@item{@emph{apps} = applications and reads}
@item{@emph{makes} = allocations}
@item{@emph{depth} = layers of chaperones}
]

In more detail:
 @emph{Proc. apps} counts the number of times the benchmark applies a chapereoned procedure,
 @emph{Struct apps} counts the number of field references or property accesses to chaperoned structs,
 and @emph{Vec. apps} counts the number of references to chaperoned vectors.
The @emph{Proc. makes}, @emph{Struct makes}, and @emph{Vec. makes} columns count
 the number of times each kind of chaperone was created.
Finally, @emph{Proc. depth}, @emph{Struct depth}, and @emph{Vec. depth} report
 the largest number of chaperones layered on top of one value.
For example, if @emph{Proc. depth} is 3 then there is at least one function
 in the benchmark that gets wrapped in three procedure chaperones when the benchmark runs.

@figure["fig:dynamic-chaperones" @elem{TWC Chaperone Details}
  @format-chaperones-info[]]


