#lang scribble/manual

@require[
  gtp-benchmarks/scribblings/util
  scriblib/figure
  scribble/example
  (for-label
    require-typed-check
    racket/base
    racket/contract
    (only-in racket/math natural?)
    (only-in require-typed-check/logging require-typed-check-logger)
    (only-in typed/racket/base require/typed))]

@title{GTP Benchmarks}

GTP = gradual typing performance

Source: @url{https://github.com/bennn/gtp-benchmarks}

This package contains benchmarks for measuring the cost of typed/untyped
 interaction in Typed Racket.

@section[#:tag "gtp:api"]{Running a benchmark}

To run a benchmark:

@itemlist[
@item{
  Create a new directory relative to the benchmark's @filepath{base/} folder (if it exists)
}
@item{
  Copy the benchmarks @filepath{both/} folder into the new directory
}
@item{
  Copy your favorite mix of @filepath{typed/} and @filepath{untyped/} modules into the new directory
}
@item{
  Run the @filepath{main.rkt} module
}
]

@defmodule[gtp-benchmarks/utilities/make-configurations]{
  Script for generating all typed/untyped configurations of a benchmark.
}

@defproc[(make-configurations [dir (and/c path-string? directory-exists?)]) void?]{
  Given a path to a benchmark (under the @filepath{benchmarks/} directory),
   generates all typed/untyped configurations of the benchmark and stores
   them in a new folder in the current directory.

  On the command-line:
  @nested[#:style 'inset @exec{racket utilities/make-configurations.rkt benchmarks/NAME}]
}


@section[#:tag "gtp:history"]{Version Notes}

@itemlist[
  @item[
    @history[#:change "1.0"
             @elem{Renamed @bm{quadBG} to @bm{quadU} and replaced @bm{quadMB} with @bm{quadT}.
                   In the beginning, @bm{quad} came to us as two programs:
                    an original untyped program and a fully-typed version by the original author.
                   The @bm{quadMB} benchmark integrated these two programs;
                    this was a @bold{BAD} decision, because the typed version performed significantly different computations due to uses of @racket[cast] and @racket[define-predicate].
                   The @bm{quadBG} benchmark added types to the untyped program (with minimal changes to the code).
                   The new @bm{quadMB} benchmark removes types from the typed program (with minimal changes to the code).
                   Please do not refer to @bm{quadBG} or @bm{quadMB} in future applications of this benchmark suite.}]]
]


@section[#:tag "gtp:descriptions"]{Benchmark Descriptions}

This section has summaries of the benchmark programs.

In each description, the @emph{author} and @emph{source} fields credit the
 authors of the code that inspired the benchmarks.
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

  Note: this benchmark runs very quickly untyped (< 50 milliseconds), and extremely slowly with certain type boundaries.
  As the extreme slowdowns improve, we plan to increase the input size so the untyped configuration runs in the 1-2 second range.

  @history[#:changed "0.2" @elem{Increased input size, thanks to Typed Racket improvements (@hyperlink["https://github.com/racket/typed-racket/commit/ff2956d031f9ccea840d56c37d4011e826c873dd"]{@tt{@smaller{ff2956d}}}).}]
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
@format-benchmark[#:name 'quadU
                  #:author "Ben Greenman"
                  #:purpose "Typesetting"
                  #:origin "https://github.com/mbutterick/quad"
                  #:depends '((csp untyped))]{
  Converts S-expression source code to @tt{PDF} format.
  TODO
}
@format-benchmark[#:name 'quadT
                  #:author "Matthew Butterick"
                  #:purpose "Typesetting"
                  #:origin "https://github.com/mbutterick/quad"
                  #:depends '((csp untyped))]{
  Converts S-expression source code to @tt{PDF} format.
  TODO
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
 @deftech[#:key "twc"]{theoretical worst-case configuration} (for short: @tech{TWC}) of each benchmark.
The @tech{TWC} configuration is the one in which every boundary between
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
 after running the @tech{TWC} configuration.
Column @emph{Milliseconds} column reports total running time (including setup, i.e., reading from data files) in milliseconds.
Column @emph{GC Milliseconds} column reports the total garbage collection time.
Column @emph{Num. GC} reports the number of garbage collections performed since start-up in the current place.
Column @emph{Peak Bytes} reports the largest number of bytes that were allocated just before a garbage collection.


@figure["fig:dynamic-time" @elem{@tech{TWC} Time Details}
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

@figure["fig:dynamic-chaperones" @elem{@tech{TWC} Chaperone Details}
  @format-chaperones-info[]]


@section[#:tag "gtp:reproducibility"]{Reproducibility}

@subsection{Module Dependence Graphs}

@defmodule[gtp-benchmarks/utilities/modulegraph]{
  Script for generate module dependence graphs.
}

@defproc[(make-modulegraph [src* (listof path-string?)]) modulegraph?]{
  Create an adjacency list of @racket[require] depedencies between the given modules.
  Uses @racket[module->imports] to collect dependencies.
}

@defproc[(modulegraph? [x any/c]) boolean?]{
  Predicate for an adjacency list.
}


@subsection{Type Information}

@defmodule[gtp-benchmarks/utilities/type-info]{
  Script for collecting the types in a benchmark
}

@defproc[(compile/require-typed-check-info [src path-string?]) (listof string?)]{
  Compiles the given module, returns all log messages generated by the
   @racket[require-typed-check-logger].
}


@subsection{Counting Chaperones}

@defmodule[gtp-benchmarks/utilities/count-chaperones]{
  Script for collecting low-level performance details.
  This script requires a version of Racket patched with special performance counters.
  There is a (possibly out-of-date) copy of the patch included with this repository.
}

@defproc[(count-chaperones [bin racket-bin/count-chaperones?] [src path-string?]) chaperones-count/c]{
  Invokes the @exec{raco} executable in the given binary folder to compile the given module,
   then uses the @exec{racket} executable in @racket[bin] to run @racket[src].
  Collects performance info using @racket[vector-set-performance-stats!] and
   the performance counters installed by the patch mentioned above.
}

@defproc[(racket-bin/count-chaperones? [x any/c]) boolean?]{
  Returns true for a directory that contains @exec{raco} and @exec{racket}
   executables with support for counting chaperones.
}

@defthing[#:kind "contract" chaperones-count/c (hash/c symbol? (or/c natural? hash?) #:immutable #true #:flat? #true)]{
  Predicate for the performance info generated by the @racket[count-chaperones] function.
}
