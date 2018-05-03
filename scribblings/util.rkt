#lang at-exp racket/base

(provide
  format-benchmark
  library
  bm
  tabulate-gradual-typing-benchmarks-size)

(require
  (only-in gtp-util
    save-pict
    rnd)
  (only-in gtp-util/system
    md5sum)
  (only-in scribble-abbrevs
    oxfordize
    integer->word
    format-url)
  (only-in racket/format
    ~r)
  (only-in syntax/modresolve
    resolve-module-path)
  (only-in racket/list
    make-list)
  (only-in racket/math
    pi
    order-of-magnitude)
  file/glob
  pict
  syntax-sloc/directory-sloc
  scribble/manual
  (except-in scribble/core
    table)
  racket/file
  racket/path
  racket/runtime-path
  racket/string
  with-cache)

(module+ test
  (require rackunit)
  (define CI? (and (getenv "CI") #true)))

;; -----------------------------------------------------------------------------

(define-runtime-path CWD ".")
(define benchmarks-path (build-path CWD ".." "benchmarks"))
(define cache-path (build-path CWD "cache"))

(define untyped-name "untyped")
(define typed-name "typed")
(define compiled-name "compiled")
(define base-name "base")
(define both-name "both")

(define-logger gtp-benchmarks)

(define (directory->md5* dir)
  (map md5sum (glob (build-path dir "**" "*.rkt"))))

(define benchmarks-md5*
  (directory->md5* benchmarks-path))

(define BENCHMARK-NAME*
  (for/list ((x (in-list (directory-list benchmarks-path))))
    (string->symbol (path->string x))))

(module+ test
  (test-case "benchmark-name*"
    (check-equal? BENCHMARK-NAME*
                  '(acquire dungeon forth fsm fsmoo gregor jpeg kcfa lnm mbta
                    morsecode quadBG quadMB sieve snake suffixtree
                    synth take5 tetris zombie zordoz))))

(define library tt)
(define bm tt)

(define (format-benchmark #:name name
                          #:author author
                          #:purpose purpose
                          #:origin origin
                          #:depends lib*
                          . descr)
  (define H (linebreak))
  (para
    (bold name)
    H
    (format-author author)
    ;; 2018-04-11 : skip the 'purpose'
    ;; H
    ;; (format-purpose purpose)
    (list H (format-origin origin))
    (linebreak)
    (format-dependencies lib*)
    (linebreak)
    descr
    (linebreak)
    (benchmark->modulegraph-pict (string->symbol name))))

(define (format-author author)
  (define more-than-one? (and (pair? author) (not (null? (cdr author)))))
  (format-key/value (format "author~a" (if more-than-one? "s" ""))
                    (if more-than-one? (oxfordize author) author)))

(define (format-purpose p)
  (format-key/value "purpose" p))

(define (format-origin str)
  (format-key/value "source"
                    (if str
                      (format-url str)
                      "?")))

(define (format-dependencies lib*)
  (format-key/value "dependencies" (if (null? lib*) "None" (oxfordize (map format-lib lib*)))))

(define format-lib
  (let ([lib/url '(("racket/list" "http://docs.racket-lang.org/reference/pairs.html")
                   ("plot" "https://docs.racket-lang.org/plot/")
                   ("math/array" "https://docs.racket-lang.org/math/array.html")
                   ("math/statistics" "https://docs.racket-lang.org/math/stats.html")
                   ("compiler/zo-parse" "http://docs.racket-lang.org/raco/decompile.html")
                   ("compiler/zo-structs" "http://docs.racket-lang.org/raco/decompile.html")
                   ("graph" "http://github.com/stchang/graph")
                   ("csp" "https://github.com/mbutterick/csp")
                   ("cldr" "https://docs.racket-lang.org/cldr-core/index.html")
                   ("rnrs/bytevectors-6" "http://docs.racket-lang.org/r6rs/R6RS_Libraries.html#%28mod-path._rnrs%2Fbytevectors-6%29")
                   ("tzinfo" "https://docs.racket-lang.org/tzinfo/index.html"))])
    (lambda (x+t)
      (define x (car x+t))
      (define t (cadr x+t))
      (define str (if (symbol? x) (symbol->string x) x))
      (define match (assoc str lib/url))
      (list
        (if match
          (hyperlink (cadr match) (tt str))
          (begin
            (printf "warning: no URL for library ~a~n" str)
            (tt str)))
        (format " (~a)" t)))))

(define (remove-prefix rx str)
  (define m (regexp-match (string-append "^" rx "(.*)$") str))
  (if m (cadr m) str))

(define (format-key/value k v)
  (smaller k ": " v))

(define (tabulate-gradual-typing-benchmarks-size)
  (define title* '("Untyped LOC" "Annotation LOC" "# Modules" "# Bnd." "# Exp."))
  (make-size-table benchmarks-path BENCHMARK-NAME* get-gradual-typing-static-info title* "gtp-size.rktd"))

(define (make-size-table dir name* benchmark->info title* file-name)
  (define name->info
    (parameterize ([*with-cache-fasl?* #false]
                   [*current-cache-directory* cache-path]
                   [*current-cache-keys* (list (lambda () benchmarks-md5*))])
      (with-cache (cachefile file-name)
        (lambda ()
          (log-gtp-benchmarks-info "building size table (ETA 10 minutes)")
          (for/list ((bm-name (in-list name*)))
            (cons bm-name (benchmark->info bm-name)))))))
  (make-table/horizontal name->info title* values))

(define (make-table/horizontal tbl title* title->key)
  (define rendered-name*
    (for/list ([kv (in-list tbl)])
      (tt (symbol->string (car kv)))))
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(left right)
      #:column-properties '(right)
      (cons (cons "" title*)
            (for/list ((kv (in-list tbl))
                       (n (in-list rendered-name*)))
              (cons n
                    (for/list ([t (in-list title*)])
                      (maybe-rnd (hash-ref (cdr kv) (title->key t))))))))))

(define (maybe-rnd x)
  (if (exact-integer? x)
    (number->string x)
    (rnd x)))

(module+ test
  (check-equal? (maybe-rnd 0) "0")
  (check-equal? (maybe-rnd 0.123) "0.12"))

(define (get-gradual-typing-static-info bm-name)
  (log-gtp-benchmarks-info "collecting static info for '~a'" bm-name)
  (define src (benchmark->typed/untyped-dir bm-name))
  (define u-loc (get-untyped-loc src))
  (define t-loc (get-typed-loc src))
  (define a-loc
    (let ([diff (- t-loc u-loc)])
      (when (< diff 0)
        (printf "warning: '~a' untyped LOC is greater than typed LOC (~a vs. ~a)~n" bm-name u-loc t-loc))
      diff))
  (define g (get-modulegraph src #true))
  (make-immutable-hash
    (list (cons "Untyped LOC" u-loc)
          (cons "Annotation LOC" a-loc)
          (cons "# Modules" (modulegraph->num-modules g))
          (cons "# Bnd." (modulegraph->num-boundaries g))
          (cons "# Exp." (modulegraph->num-exports g)))))

(define (benchmark->typed/untyped-dir bm-name)
  (define src (build-path benchmarks-path (symbol->string bm-name)))
  (unless (directory-exists? src)
    (printf "warning: benchmark directory does not exist '~a'~n" (path->string src)))
  src)

(define (get-untyped-loc src)
  (directory-sloc (build-path src untyped-name)))

(define (get-typed-loc src)
  (directory-sloc (build-path src typed-name)))

(define (benchmark->modulegraph-pict bm-name)
  (define G (benchmark->modulegraph bm-name))
  (define-values [name-key pict] (modulegraph->pict G string<?))
  ;; TODO something nicer with the key
  pict
  )

(define (benchmark->modulegraph bm-name)
  (hash-ref modulegraph-cache bm-name))

(define (get-modulegraph src [only-in-project? #true])
  (void
    (clean-directory src))
  (define udir (build-path src untyped-name))
  (define mod* (glob (build-path udir "*.rkt")))
  (define in-project?
    (if only-in-project?
      (lambda (path) (and (member path mod*) #true))
      (lambda (path) #true)))
  (for/list ([src (in-list mod*)])
    (define dst* (complete-path->imported-modules src))
    (cons src (filter in-project? dst*))))

(define (clean-directory dir)
  (log-gtp-benchmarks-info "cleaning directory '~a'" dir)
  ;; TODO can also do `(setup #:clean? #true ....)` from `setup/setup`
  (for ((d (in-list (glob (build-path dir "**" compiled-name)))))
    (delete-directory/files d #:must-exist? #false)))

(define (modulegraph->num-modules mg)
  (length (modulegraph->modules mg)))

(define (modulegraph->modules mg)
  (map car mg))

(define (modulegraph->num-externals mg)
  (length (modulegraph->externals mg)))

(define (modulegraph->externals mg)
  (define m* (modulegraph->modules mg))
  (for*/list ((src+dst* (in-list mg))
              (dst (in-list (cdr src+dst*)))
              #:when (not (member dst m*)))
    dst))

(define (modulegraph->num-boundaries mg)
  (for/sum ([src+dst* (in-list mg)])
    (length (cdr src+dst*))))

(define (modulegraph->num-exports mg)
  (for/sum ([src+dst* (in-list mg)])
    (length (complete-path->exported-identifiers (car src+dst*)))))

(define (complete-path->imported-modules path)
  (define parent-dir (path-only path))
  (define all-imports
    (with-visit-namespace path module->imports))
  (let loop ([imp* all-imports])
    (if (null? imp*)
      '()
      (let ()
        (define mpi-name*
          (for/list ([x (in-list (car imp*))]
                     #:when (module-path-index? x))
            (define-values [name _] (module-path-index-split x))
            name))
        (append (for/list ([mpi-name (in-list mpi-name*)]
                           #:when (string? mpi-name))
                  (build-path parent-dir mpi-name))
                (loop (cdr imp*)))))))

(define (complete-path->exported-identifiers path)
  (define-values [p* s*]
    (with-visit-namespace path module->exports))
  (append (parse-provided p*) (parse-provided s*)))

(define (with-visit-namespace path f)
  (define r (resolve-module-path path #false))
  (parameterize ((current-namespace (make-base-namespace)))
    (dynamic-require r (void))
    (f r)))

(define (parse-provided x*)
  (apply append
    (for/list ([x (in-list x*)]
               #:when (and (car x) (zero? (car x))))
      (map car (cdr x)))))

(define (modulegraph-tsort adj [name<? string<?])
  (define m* (modulegraph->modules adj))
  (define indegree-map
    (make-hash (for/list ([src+dst* (in-list adj)])
                 (cons (car src+dst*)
                       (for/sum ((dst (in-list (cdr src+dst*)))
                                 #:when (member dst m*))
                         1)))))
  (reverse
    (let loop ([acc '()])
      (cond
       [(zero? (hash-count indegree-map))
        acc]
       [else
        (define zero-indegree*
          (for/list ([(k v) (in-hash indegree-map)]
                     #:when (zero? v)) k))
        (for ([k (in-list zero-indegree*)])
          (hash-remove! indegree-map k)
          (define src* (modulegraph->direct-ancestor* adj k))
          (for ([src (in-list src*)])
            (hash-set! indegree-map src
              (- (hash-ref indegree-map src (lambda () -1)) 1))))
            ;(hash-update! indegree-map src sub1 -1)))
        (loop (cons (sort zero-indegree* name<?) acc))]))))

(define (modulegraph->direct-ancestor* mg k)
  (for/list ([src+dst* (in-list mg)]
             #:when (member k (cdr src+dst*)))
    (car src+dst*)))

(define (modulegraph->pict mg [name<? string<?])
  (define W 20)
  (define e* (modulegraph->externals mg))
  (define name-key (make-hash))
  (define name->string
    (let ([num (box 1)]
          [oom (+ 1 (order-of-magnitude (+ (modulegraph->num-modules mg) (length e*))))])
      (lambda (name)
        (if name
          (let ()
            (define curr (unbox num))
            (set-box! num (+ curr 1))
            (define v (~r curr #:min-width oom #:pad-string " "))
            (hash-set! name-key name v)
            v)
          #false))))
  (define m** (modulegraph-tsort mg name<?))
  (define (level x)
    (for/first ([m* (in-list m**)]
                [i (in-naturals 0)]
                #:when (member x m*))
      i))
  (define (level-diff src dst)
    (define l-src (level src))
    (define l-dst (level dst))
    (if (and l-src l-dst)
      (- (- l-dst l-src) 1)
      0))
  (define num-cols (length m**))
  (define num-rows (apply max (map length m**)))
  (define m+pict
    (for/list ([name (in-list (transpose+append (map (pad-list num-rows #false) m**)))])
      (cons name (render-module (name->string name) W))))
  (define grid-base
    (table num-cols (map cdr m+pict) cc-superimpose cc-superimpose (* 2 W) W))
  (define e+pict
    (for/list ([e (in-list e*)])
      (cons e (render-external (name->string e) W))))
  (define grid+lib
    (vc-append W grid-base (apply hc-append W (map cdr e+pict))))
  (define (name->pict name)
    (define r0 (assoc name m+pict))
    (if r0
      (cdr r0)
      (cdr (assoc name e+pict))))
  (define grid-arr
    (for*/fold ((acc grid+lib))
               ((src+dst* (in-list mg))
                (dst (in-list (cdr src+dst*))))
      (define src (car src+dst*))
      (define src-pict (name->pict src))
      (define dst-pict (name->pict dst))
      (define angle (* (/ pi 4) (level-diff dst src)))
      (if (member dst e*)
        (pin-arrow-line 10 acc src-pict cb-find dst-pict ct-find)
        (pin-arrow-line 10 acc dst-pict rc-find src-pict lc-find
                        #:start-angle (- angle)
                        #:end-angle angle))))
  (values name-key grid-arr))

(define (transpose+append x**)
  (if (null? x**)
    '()
    (let loop ((x** x**))
      (if (ormap null? x**)
        '()
        (append (map car x**) (loop (map cdr x**)))))))

(define ((pad-list min-length x) x*)
  (append x* (make-list (max 0 (- min-length (length x*))) x)))

(define (render-module str size)
  (if str
    (let ((txt (render-text str)))
      (cc-superimpose (circle (* 1.5 size)) txt))
    (blank 0 0)))

(define (render-external str size)
  (define size++ (* size 1.4))
  (if str
    (let ((txt (render-text str)))
      (cc-superimpose (rectangle size++ size++) txt))
    (blank 0 0)))

(define (render-text str)
  (text str '(bold) 22))

(define (modulegraph-map f mg)
  (for/list ((x* (in-list mg)))
    (for/list ((x (in-list x*)))
      (f x))))

(define (simplify-module-names G base)
  (define tu (build-path base "untyped"))
  (modulegraph-map (simple-module-name tu) G))

(define ((simple-module-name base) m)
  (define str-base (path->string (simplify-path base)))
  (define str-m (path->string m))
  (if (string-prefix? str-m str-base)
    (substring str-m (+ 1 (string-length str-base)))
    str-m))

(define modulegraph-cache
  (parameterize ([*with-cache-fasl?* #false]
                 [*current-cache-directory* cache-path]
                 [*current-cache-keys* (list (lambda () benchmarks-md5*))])
    (with-cache (cachefile "modulegraph.rktd")
      (lambda ()
        (for/hash ((bm (in-list BENCHMARK-NAME*)))
          (define tu (benchmark->typed/untyped-dir bm))
          (define G (get-modulegraph tu #false))
          (values bm (simplify-module-names G tu)))))))

;; =============================================================================

(module+ test

  (require racket/set)

  (test-case "benchmark->typed/untyped-dir"
    (check-equal? (benchmark->typed/untyped-dir 'sieve) (build-path benchmarks-path "sieve"))
    (check-equal? (benchmark->typed/untyped-dir 'gregor) (build-path benchmarks-path "gregor")))

  (test-case "get-untyped-loc"
    (check-equal? (get-untyped-loc (benchmark->typed/untyped-dir 'dungeon)) 541))

  (test-case "get-typed-loc"
    (check-equal? (get-typed-loc (benchmark->typed/untyped-dir 'dungeon)) 610))

  (test-case "modulegraph:synthetic"
    (define G '(("A" "B" "C")
                ("B" "C" "D")
                ("C")
                ("E")))
    (check-equal? (modulegraph->modules G) '("A" "B" "C" "E"))
    (check-equal? (modulegraph->num-boundaries G) 4)
    (check-equal? (modulegraph->externals G) '("D"))
    (check-equal? (modulegraph->direct-ancestor* G "A") '())
    (check-equal? (modulegraph->direct-ancestor* G "B") '("A"))
    (check-equal? (modulegraph->direct-ancestor* G "C") '("A" "B"))
    (check-equal? (modulegraph-tsort G) '(("C" "E") ("B") ("A"))))

  (test-case "transpose+append"
    (check-equal? (transpose+append '()) '())
    (check-equal? (transpose+append '((A) (B) (C))) '(A B C))
    (check-equal? (transpose+append '((A D) (B E) (C F))) '(A B C D E F)))

  (test-case "pad-list"
    (check-equal? ((pad-list 0 'X) '()) '())
    (check-equal? ((pad-list 0 'X) '(A B)) '(A B))
    (check-equal? ((pad-list 5 'X) '(A B)) '(A B X X X)))

  (test-case "modulegraph:real"
    (define (test-modulegraph bm-name expected-num-modules expected-num-boundaries expected-num-exports)
      (define mg (get-modulegraph (benchmark->typed/untyped-dir bm-name)))
      (check-equal? (modulegraph->num-modules mg) expected-num-modules)
      (check-equal? (modulegraph->num-boundaries mg) expected-num-boundaries)
      (check-equal? (modulegraph->num-exports mg) expected-num-exports)
      (void))
    (test-modulegraph 'sieve 2 1 9)
    (test-modulegraph 'morsecode 4 3 15)
    (test-modulegraph 'snake 8 16 31)
    (let ([g (get-modulegraph (benchmark->typed/untyped-dir 'mbta) #false)])
      (define tu (benchmark->typed/untyped-dir 'mbta))
      (check-equal? (modulegraph->num-modules g) 4)
      (check-equal? (modulegraph->num-boundaries g) 4)
      (check-equal? (modulegraph->num-externals g) 1)
      (check-equal? (simplify-module-names g tu)
                    '(("main.rkt" "run-t.rkt")
                      ("run-t.rkt" "t-view.rkt")
                      ("t-graph.rkt" "../base/my-graph.rkt")
                      ("t-view.rkt" "t-graph.rkt")))))

  (test-case "complete-path->imported-modules"
    (let ([sieve-main (build-path (benchmark->typed/untyped-dir 'sieve) untyped-name "main.rkt")]
          [sieve-streams (build-path (benchmark->typed/untyped-dir 'sieve) untyped-name "streams.rkt")])
      (check-equal? (complete-path->imported-modules sieve-main) (list sieve-streams))))

  (test-case "complete-path->exported-identifiers"
    (let* ([sieve-streams (build-path (benchmark->typed/untyped-dir 'sieve) untyped-name "streams.rkt")]
           [exp* (complete-path->exported-identifiers sieve-streams)])
      (check set=? exp*
             '(make-stream stream-first stream-get stream-rest stream-take stream-unfold stream? struct:stream stream))))

  (test-case "directory->md5*"
    (let ([sieve (build-path benchmarks-path "sieve")])
      (check set=?
        (directory->md5* sieve)
        (list (md5sum (build-path sieve typed-name "main.rkt"))
              (md5sum (build-path sieve typed-name "streams.rkt"))
              (md5sum (build-path sieve untyped-name "main.rkt"))
              (md5sum (build-path sieve untyped-name "streams.rkt"))))))

  (test-case "get-gradual-typing-static-info"
    (let ((si (get-gradual-typing-static-info 'sieve)))
      (check-equal? (hash-ref si "Untyped LOC") 35)
      (check-equal? (hash-ref si "Annotation LOC") 17)
      (check-equal? (hash-ref si "# Modules") 2)
      (check-equal? (hash-ref si "# Bnd.") 1)
      (check-equal? (hash-ref si "# Exp.") 9))
    (let ((zi (get-gradual-typing-static-info 'zombie)))
      (check-equal? (hash-ref zi "Untyped LOC") 300)
      (check-equal? (hash-ref zi "Annotation LOC") 25)
      (check-equal? (hash-ref zi "# Modules") 4)
      (check-equal? (hash-ref zi "# Bnd.") 3)
      (check-equal? (hash-ref zi "# Exp.") 15)))
)
