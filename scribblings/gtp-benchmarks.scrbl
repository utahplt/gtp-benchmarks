#lang scribble/manual

@require[
  gtp-benchmarks/scribblings/util
  scriblib/figure
  scribble/example
  pict
  racket/runtime-path
  (only-in setup/getinfo get-info)
  (for-label
    require-typed-check
    racket/base
    racket/contract
    gtp-benchmarks/utilities/count-chaperones
    (only-in racket/function curry)
    (only-in racket/list argmax)
    (only-in racket/math natural?)
    (only-in require-typed-check/logging require-typed-check-logger)
    (only-in typed/racket/base require/typed))]

@(define (package-version)
   (define v-str ((get-info '("gtp-benchmarks")) 'version))
   (unless (string? v-str)
     (raise-argument-error 'gtp-benchmarks "valid-version?" v-str))
   v-str)

@; -----------------------------------------------------------------------------

@title{GTP Benchmarks}
@section-index{gtp-benchmarks}

GTP = gradual typing performance

Latest Version: @hyperlink["https://github.com/bennn/gtp-benchmarks/releases"]{@package-version[]}

Source: @url{https://github.com/bennn/gtp-benchmarks}

This package contains benchmarks for measuring the cost of typed/untyped
 interaction in Typed Racket @cite{REP}.

@bold{Always include a version number when reporting data on these benchmarks.}

@(define-runtime-path img "./img/")
@(let* ((n-pict 6)
        (ww 100)
        (sep 10)
        (base (blank (+ (* ww n-pict) (* sep (- n-pict 1))) (* 10/9 ww)))
        (pp* (for/list ((i (in-range n-pict))) (scale-to-fit (bitmap (build-path img (format "gtp-~a.png" i))) ww ww))))
   (cc-superimpose base (apply hc-append sep pp*)))

@section[#:tag "gtp:api"]{Running a benchmark}

There are three ways to run a benchmark:
either by copying code,
using the @racketmodname[gtp-measure] package,
or running a setup script.


@subsection[#:tag "gtp:quick-run"]{Quick Route}

@itemlist[
@item{
  Create a new directory relative to the benchmark's @filepath{base/} folder (if it exists)
}
@item{
  Copy the benchmark's @filepath{both/} folder into the new directory
}
@item{
  Copy your favorite mix of @filepath{typed/} and @filepath{untyped/} modules into the new directory
}
@item{
  Run the @filepath{main.rkt} module
}
]


@subsection[#:tag "gtp:measure-run"]{Official Route}

@itemlist[
@item{
  Install the @hyperlink["https://docs.racket-lang.org/gtp-measure/index.html"]{@tt{gtp-measure}} package
}
@item{
  Make a copy of the
  @hyperlink["https://github.com/bennn/gtp-benchmarks/tree/master/utilities/sample-gtp-measure-manifest.rkt"]{sample benchmarking script}
  in this repo and modify it to match your machine / goals.
}
@item{
  Run via:
    @nested[#:style 'inset @exec|{PLTSTDERR="error info@gtp-measure" raco gtp-measure --output sample-data/ sample-gtp-measure-manifest.rkt}|]
}
]


@subsection[#:tag "gtp:script-run"]{Semi-Auto Route}

@itemlist[
@item{
  On the command-line:
    @nested[#:style 'inset @exec{racket utilities/make-configurations.rkt benchmarks/NAME}]
  This creates a directory with all typed/untyped configurations.
}
@item{
  Move to a configuration sub-directory and run the @filepath{main.rkt} module.
}
]

@defmodule[gtp-benchmarks/utilities/make-configurations]{
  Script for generating all typed/untyped configurations of a benchmark.
}

@defproc[(make-configurations [dir (and/c path-string? directory-exists?)]) void?]{
  Given a path to a benchmark (under the @filepath{benchmarks/} directory),
   generates all typed/untyped configurations of the benchmark and stores
   them in a new folder in the current directory.
}


@section[#:tag "gtp:history"]{Version Notes}

See also the GitHub release notes:
@url{https://github.com/bennn/gtp-benchmarks/releases}

@itemlist[
  @item[
    @history[#:changed "9.2"
             @elem{Add an @racket[assert] in @racket[take5] to accommodate the
                   improved type of @racket[random] in Racket v8.9
                   (@hyperlink["https://github.com/racket/typed-racket/commit/246173a67"]{@tt{246173a67}}).
                   This change affects the typed and untyped configurations.}]]
  @item[
    @history[#:changed "9.1"
             @elem{In @racket[take5], replace the @racket[(module+ main expr)] with
                   the unwrapped expression @racket[expr]. This matches the structure
                   of other benchmarks and makes it easier to run tools like
                   @other-doc['(lib "profile/scribblings/profile.scrbl") #:indirect "statistical profiler"]
                   and @other-doc['(lib "contract-profile/scribblings/contract-profile.scrbl") #:indirect "contract profiler"],
                   which do not check submodules by default.}]]
  @item[
    @history[#:changed "9.0"
             @elem{Substantially revise @racket[acquire] and @racket[take5].
                   Before, @racket[acquire] ran a game with AI players that all raised
                   exceptions and @racket[take5] ignored an input list of AI players.
                   After, the @racket[acquire] players make valid moves and
                   @racket[take5] uses its input. These changes have little impact
                   on typed/untyped overhead.}]]
  @item[
    @history[#:changed "8.0"
             @elem{Remove @racket[racket/sandbox] dependency from @bm{acquire}
                   and change the benchmark to stop using a player AI that times out.
                   Timing info from sandbox depends on system calls; measurements should
                   be more stable without it.
                   Based on a first measurement, the typed/untyped overhead in @racket[acquire]
                   is the same before and after.}]]
  @item[
    @history[#:changed "7.0"
             @elem{Fix a typed/untyped mismatch in @bm{lnm}; both versions of a
                   certain helper function return @racket[(void)] now.}]]
  @item[
    @history[#:changed "6.0"
             @elem{@bold{Major Release}.
                   Edited all benchmarks to match up typed and untyped code.
                   Typed code now uses @racket[assert] instead of @racket[cast]
                    and untyped code uses an untyped version of the same
                    predicate function.
                   Reordered functions / methods so that a per-file diff lines
                    up; the only differences between files now should be
                    types and @racket[require] forms.

                   After, @bm{lnm} has lower overhead because of new asserts
                    in untyped code.
                   Both @bm{quadU} and @bm{quadT} have higher overhead,
                    possibly because Typed Racket fixed a soundness hole in
                    the meantime (@hyperlink["https://github.com/racket/typed-racket/commit/1643443502a54d557e4043de0d7e0b9a5e41ba7c"]{1643443}).}]]
  @item[
    @history[#:changed "5.0"
             @elem{(1) Remove an unused call to @racket[format] in the typed
                   version of @bm{zordoz}. This change @bold{significantly}
                   improves the runtime of typed code; for example, the
                   typed/untyped ratio improves by an order of magnitude.
                   @linebreak[]

                   (2) Fix an unbound identifier error in the typed version of
                   @bm{lnm}. This error went undiscovered because the @tt{plot}
                   library caught and ignored it, but does not change overall
                   performance.}]]
  @item[
    @history[#:changed "4.0"
             @elem{Replace a high-cost cast in @bm{zombie} with a predicate,
                   and use the same predicate in untyped code. This change
                   makes the benchmark a better test for "the cost of mixing
                   typed and untyped code" by removing an un-equal computation
                   between the untyped and typed versions. (It would be good
                   to reduce the run-time cost of casts, but that's not the
                   main concern of this benchmark suite.)}]]
  @item[
    @history[#:changed "3.0"
             @elem{Fixed an issue in the untyped @bm{zordoz} code.
                   Before, the untyped code imported the typed version of the @tt{compiler/zo-structs} library.
                   After, the untyped code avoids this unnecessary type boundary.
                   This issue seriously affects the conclusion about @bm{zordoz}
                    reached in @cite{JFP-2019}.
                   That paper compares one version of @bm{zordoz} that does not
                    suffer the issue (6.2) against two that do (6.3 and 6.4),
                    and incorrectly concludes that changes to Typed Racket
                    improved the overhead in the later versions.
                   In fact, the overhead is better because the untyped code
                    got unnecessarily slower.
                   More at: @url{https://github.com/bennn/gtp-benchmarks/issues/16}.  }]]
  @item[
    @history[#:changed "2.0"
             @elem{Fixed a difference between the typed and untyped @bm{mbta} code.
                   Before, untyped created a function @racket[(curry argmax f)].
                   After, both create a function @racket[(lambda (l) ((curry argmax f) l))].}]]
  @item[
    @history[#:changed "1.0"
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
                  #:origin "https://github.com/stamourv/dungeons-and-repossessions"
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
  Simulates an economy with finite-state automata.
  The economy is implemented as a vector; this vector repeatedly crosses between
  modules in the benchmark.

  @racket[fsmoo] is similar, but implements the economy as an object.
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
  specifically, it analyzes an encoding of @tt{(2 * (1 + 3)) = (1 * 2)}
  (provided by @hyperlink["https://github.com/deeglaze"]{Dee Glaze}
   via @cite{OAAM}
   @hyperlink["https://github.com/dvanhorn/oaam/blob/master/benchmarks/church.sch"]{(church-num)}
   via @cite{CFA2}).
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
  This benchmark started from an @bold{untyped} program by the original author.
  For the benchmark, we @bold{added} types with minimal changes to the code.
}
@format-benchmark[#:name 'quadT
                  #:author "Matthew Butterick"
                  #:purpose "Typesetting"
                  #:origin "https://github.com/mbutterick/quad"
                  #:depends '((csp untyped))]{
  Converts S-expression source code to @tt{PDF} format.
  This benchmark started from a @bold{typed} program by the original author.
  For the benchmark, we @bold{removed} types with minimal changes to the code.
  Any @tt{cast} forms changed to analogous @tt{contract} forms, and any
  @tt{define-predicate} forms changed to functions or contracts.
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
 and is adapted from a patch by @hyperlink["http://cs.brown.edu/~sstrickl/chaperones/"]{Strickland, Tobin--Hochstadt, Findler, and Flatt (2012)}.


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

When rebuilding this document, subscribe to the @litchar{gtp-benchmarks} logger
 for information about the build:

@nested[#:style 'inset @exec|{PLTSTDERR="error info@gtp-benchmarks" raco setup gtp-benchmarks}|]


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

@defproc[(complete-path->imported-modules [p path-string?]) (listof complete-path?)]{
  Uses @racket[module->imports] to build a list of one file's imports.

  @history[#:added "5.1"]
}

@defproc[(complete-path->exported-identifiers [p path-string?]) (listof symbol?)]{
  Uses @racket[module->exports] to collect the names of one file's exports.

  @history[#:added "5.1"]
}


@subsection{Size Information}

@defmodule[gtp-benchmarks/utilities/size-info]{
  Script for counting size and dependencies.
}


@defproc[(benchmark-size-info [name symbol?]) hash?]{
  Count size statistics for a benchmark.
  Return a hash of labeled values.

  @examples[
    #:eval (make-base-eval '(require gtp-benchmarks/utilities/size-info))
    #:once
    (benchmark-size-info 'zordoz)
  ]

  @history[#:added "9.2.1"]
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

@(let ()
   (define (bib-note . str*)
     (list (linebreak) (linebreak) str* (linebreak) (linebreak)))

   (define DLS "Dynamic Languages Symposium")
   (define SFP "Workshop on Scheme and Functional Programming")
   (define POPL "Symposium on Principles of Programming Languages")
   (define PEPM "Workshop on Partial Evaluation and Program Manipulation")
   (define ESOP "European Symposium on Programming")
   (define ICFP "International Conference on Functional Programming")
   (define PHD-DISSERTATION "Ph.D. dissertation")
   (define PLDI "Conference on Programming Language Design and Implementation")
   (define OOPSLA "Conference on Object-Oriented Programming, Systems, Languages, and Applications")
   (define PADL "International Symposium on Practical Aspects of Declarative Languages")
   (define ECOOP "European Conference on Object-Oriented Programming")
   (define SNAPL "Summit oN Advances in Programming Languages")
   (define JFP "Journal of Functional Programming")
   (define REP "1st ACM Conference on Reproducibility and Replicability")

   (bibliography #:tag "gtp-bibliography"
     (bib-entry #:key "REP-2023"
                #:author "Ben Greenman"
                #:title "GTP Benchmarks for Gradual Typing Performance"
                #:location REP
                #:date "2023"
                #:url "https://doi.org/10.1145/3589806.3600034")
     (bib-entry #:key "JFP-2019"
                #:author "Ben Greenman and Asumu Takikawa and Max S. New and Daniel Feltey and Robert Bruce Findler and Jan Vitek and Matthias Felleisen"
                #:title "How to evaluate the performance of gradual type systems"
                #:location JFP
                #:date "2019"
                #:url "https://www.cambridge.org/core/journals/journal-of-functional-programming/article/how-to-evaluate-the-performance-of-gradual-type-systems/DC765724C52A3A462F16C7FB3AD18697"
                #:note @bib-note{
                  Summarizes the evaluation method; introduces the GTP benchmarks.
                })
     (bib-entry #:key "PEPM-2018"
                #:author "Ben Greenman and Zeina Migeed"
                #:title "On the Cost of Type-Tag Soundness"
                #:location PEPM
                #:date "2018"
                #:url "https://www2.ccs.neu.edu/racket/pubs/pepm18-gm.pdf"
                #:note @bib-note{
                  Generalizes the evaluation method for a study of Reticulated Python.
                  Introduces a method to approximate the number of good
                  configurations in a mixed-typed program
                  @hyperlink["http://prl.ccs.neu.edu/blog/2018/05/08/sampling-gradual-typing-performance/"]{[blog post]}.})
     (bib-entry #:key "POPL-2016"
                #:author "Asumu Takikawa and Daniel Feltey and Ben Greenman and Max S. New and Jan Vitek and Matthias Felleisen"
                #:title "Is Sound Gradual Typing Dead?"
                #:location POPL
                #:date "2016"
                #:url "https://www2.ccs.neu.edu/racket/pubs/popl16-tfgnvf.pdf"
                #:note @bib-note{
                  Introduces a method to evaluate the performance of a gradual
                  typing system and applies the method to Typed Racket.
                })
     (bib-entry #:key "OAAM"
                #:author "J. Ian Johnson, Nicholas Labich, Matthew Might, and David Van Horn"
                #:title "Optimizing Abstract Abstract Machines"
                #:location ICFP
                #:date "2015"
                #:url "https://dl.acm.org/citation.cfm?id=2500604")
     (bib-entry #:key "CFA2"
                #:author "Dimitrios Vardoulakis and Olin Shivers"
                #:title "CFA2: a Context-Free approach to Control-Flow analysis"
                #:location ESOP
                #:date "2011"
                #:url "https://link.springer.com/content/pdf/10.1007%2F978-3-642-11957-6_30.pdf")))
      
