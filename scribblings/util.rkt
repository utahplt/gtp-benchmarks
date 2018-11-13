#lang at-exp racket/base

(provide
  format-benchmark
  format-require-typed-check-info
  format-chaperones-info
  format-time-info
  library
  bm
  tabulate-gradual-typing-benchmarks-size)

(require
  (only-in gtp-util
    save-pict
    columnize
    rnd
    copy-file*
    copy-directory/files*
    copy-racket-file*)
  (only-in gtp-util/system
    md5sum)
  (only-in scribble-abbrevs
    add-commas
    oxfordize
    integer->word
    format-url)
  (only-in racket/format
    ~a
    ~r)
  (only-in racket/list
    make-list)
  (only-in racket/math
    pi
    exact-ceiling
    order-of-magnitude)
  (only-in racket/pretty
    pretty-format)
  file/glob
  gtp-benchmarks/utilities/modulegraph
  gtp-benchmarks/utilities/type-info
  gtp-benchmarks/utilities/count-chaperones
  pict
  syntax-sloc/directory-sloc
  scribble/manual
  (except-in scribble/core
    table)
  racket/file
  racket/path
  racket/runtime-path
  racket/set
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
(define config-name "configuration")
(define main-name "main.rkt")
(define staging-name "staging")

(define staging-path (build-path cache-path staging-name))
(define staging/base-path (build-path staging-path base-name))
(define staging/config-path (build-path staging-path config-name))

(module+ test
  (test-case "staging-path"
    (check-pred directory-exists? staging-path)
    (check-pred directory-exists? staging/base-path)
    (check-pred directory-exists? staging/config-path)))

(define-logger gtp-benchmarks)

(define (directory->md5* dir)
  (map md5sum (glob (build-path dir "**" "*.rkt"))))

(define BENCHMARK-NAME*
  (for/list ((x (in-list (directory-list benchmarks-path))))
    (string->symbol (path->string x))))

(define benchmarks-key*
  (sort BENCHMARK-NAME* symbol<?)
  #;(directory->md5* benchmarks-path))

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
  (list
    (subsection (format "~a Description" name))
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
    (benchmark->modulegraph-pict name)))

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
        (smaller
          (if match
            (hyperlink (cadr match) (tt str))
            (begin
              (printf "warning: no URL for library ~a~n" str)
              (tt str))))
        (smaller (format " (~a)" t))))))

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
                   [*current-cache-keys* (list (lambda () benchmarks-key*))])
      (with-cache (cachefile file-name)
        (lambda ()
          (log-gtp-benchmarks-info "building size table (ETA 10 minutes)")
          (for/list ((bm-name (in-list name*)))
            (cons bm-name (benchmark->info bm-name)))))))
  (make-table/horizontal name->info title* values maybe-rnd))

(define (make-table/horizontal tbl title* title->key render-value)
  (define rendered-name*
    (for/list ([kv (in-list tbl)])
      (tt (symbol->string (car kv)))))
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '((left bottom-border) right)
      #:column-properties '(right)
      (cons (cons "Benchmark" title*)
            (for/list ((kv (in-list tbl))
                       (n (in-list rendered-name*)))
              (cons n
                    (for/list ([t (in-list title*)])
                      (render-value (hash-ref (cdr kv) (title->key t))))))))))

(define (maybe-rnd x)
  (if (exact-integer? x)
    (number->string x)
    (rnd x)))

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
  (define g (get-modulegraph src))
  (begin0
    (make-immutable-hash
      (list (cons "Untyped LOC" u-loc)
            (cons "Annotation LOC" a-loc)
            (cons "# Modules" (modulegraph->num-modules g))
            (cons "# Bnd." (modulegraph->num-internal-boundaries g))
            (cons "# Exp." (modulegraph->num-internal-exports g))))
    (clean-staging!)))

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
  (define nt (modulegraph->name-table G #:node<? string<? #:num-columns 4))
  (define pict (modulegraph->pict G string<?))
  (list
    pict
    (format-module-names nt)))

;; format-module-names : (HashTable String Natural) -> element?
(define (format-module-names nt)
  (define (render-one kv)
    (if kv
      (format "~a. ~a" (cdr kv) (car kv))
      ""))
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(left)
      (for/list ((kv* (in-list nt)))
        (for/list ((kv (in-list kv*)))
          (smaller (render-one kv)))))))

(define (modulegraph->name-table G #:node<? [name<? string<?] #:num-columns [num-columns 2])
  (define nn (modulegraph->numbered-names G #:node<? name<?))
  (define nn+ (sort (hash->list nn) < #:key cdr))
  (rowize nn+ num-columns))

(define (rowize x* num-columns)
  (define num-in-col (exact-ceiling (/ (length x*) num-columns)))
  (define col*
    (let loop ((x* x*))
      (if (null? x*)
        '()
        (let-values (((hd* tl*) (maybe-split-at x* num-in-col)))
          (cons hd* (loop tl*))))))
  (define (maybe-car x*)
    (if (null? x*)
      #false
      (car x*)))
  (define (maybe-cdr x*)
    (if (null? x*)
      '()
      (cdr x*)))
  (let loop ((col* col*))
    (if (andmap null? col*)
      '()
      (cons (map maybe-car col*)
            (loop (map maybe-cdr col*))))))

(define (maybe-split-at x* n)
  (if (zero? n)
    (values '() x*)
    (if (null? x*)
      (values x* '())
      (let-values (((a b) (maybe-split-at (cdr x*) (- n 1))))
        (values (cons (car x*) a) b)))))

(define (benchmark->modulegraph bm-name)
  (hash-ref modulegraph-cache bm-name))

(define (modulegraph->pict mg [name<? string<?])
  (define W 20)
  (define name->string
    (let* ([name->number (modulegraph->numbered-names mg)]
           [max-num (hash-count name->number)]
           [oom (+ 1 (order-of-magnitude max-num))])
      (lambda (name)
        (if name
          (~r (hash-ref name->number name) #:min-width oom #:pad-string " ")
          #false))))
  (define m**
    ;; m** = all module names, grouped by dependencies, order from most deps to least
    (reverse (modulegraph-tsort mg #:node<? name<?)))
  (define (level x)
    (for/first ([m* (in-list m**)]
                [i (in-naturals 0)]
                #:when (member x m*))
      i))
  (define num-cols (length m**))
  (define num-rows (apply max (map length m**)))
  (define e* (modulegraph->externals mg))
  (define m+pict
    (for/list ([name (in-list (transpose+append (map (pad-list num-rows #false) m**)))])
      (cons name (render-module (name->string name) W))))
  (define grid-base
    (table num-cols (map cdr m+pict) cc-superimpose cc-superimpose (* 2 W) W))
  (define e+pict
    (for/list ([e (in-list e*)])
      (cons e (render-external (name->string e) W))))
  (define grid+lib
    (vc-append W
               grid-base
               (if (null? e+pict)
                 (blank 0 W)
                 (apply hc-append W (map cdr e+pict)))))
  (define (name->pict name)
    (define r0 (assoc name m+pict))
    (if r0
      (cdr r0)
      (cdr (assoc name e+pict))))
  (define grid-arr
    (let ([arrow-size 6]
          [angle-unit (/ (/ pi 2) num-cols)])
      (for*/fold ((acc grid+lib))
                 ((src+dst* (in-list mg))
                  (dst (in-list (cdr src+dst*))))
        (define src (car src+dst*))
        (define src-pict (name->pict src))
        (define dst-pict (name->pict dst))
        (if (member dst e*)
          (pin-arrow-line arrow-size acc src-pict cb-find dst-pict ct-find)
          (let ([angle (* angle-unit (- (level dst) (level src) 1))])
            (pin-arrow-line arrow-size acc src-pict rc-find dst-pict lc-find
                            #:start-angle (- angle)
                            #:start-pull 0.4
                            #:end-pull 0.4
                            #:end-angle angle))))))
  grid-arr)

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
    (substring str-m (+ (string-length str-base) (if (directory-string? str-base) 1 0)))
    str-m))

(define (directory-string? str)
  (define L (string-length str))
  (and (< 0 L)
       (eq? #\\ (string-ref str (- L 1)))))

(define (get-modulegraph src)
  (void
    (clean-directory! src))
  (define udir (build-path src untyped-name))
  (make-modulegraph (glob (build-path udir "*.rkt"))))

(define (clean-directory! dir)
  (log-gtp-benchmarks-info "cleaning directory '~a'" dir)
  ;; TODO can also do `(setup #:clean? #true ....)` from `setup/setup`
  (for ((d (in-list (glob (build-path dir "**" compiled-name)))))
    (delete-directory/files d #:must-exist? #false)))

(define (setup-typed-configuration! tu-dir)
  (setup-configuration! tu-dir typed-name))

(define (setup-untyped-configuration! tu-dir)
  (setup-configuration! tu-dir untyped-name))

(define (setup-configuration! tu-dir x-name)
  (define x-dir (build-path tu-dir x-name))
  (define base-dir (build-path tu-dir base-name))
  (define both-dir (build-path tu-dir both-name))
  (void
    (copy-racket-file* x-dir staging/config-path))
  (when (directory-exists? base-dir)
    (copy-directory/files* base-dir staging/base-path))
  (when (directory-exists? both-dir)
    (copy-file* both-dir staging/config-path))
  (void))

(define (clean-staging!)
  (for ((dir (in-list (list staging/base-path staging/config-path))))
    (for ((fn (in-list (glob (build-path dir "*"))))
          #:unless (bytes=? (path->bytes (file-name-from-path fn)) #"README.md"))
      (delete-directory/files fn))))

(define modulegraph-cache
  (parameterize ([*with-cache-fasl?* #false]
                 [*current-cache-directory* cache-path]
                 [*current-cache-keys* (list (lambda () benchmarks-key*))])
    (with-cache (cachefile "modulegraph.rktd")
      (lambda ()
        (log-gtp-benchmarks-info "collecting module graphs (ETA 30 minutes)")
        (for/hash ((bm (in-list BENCHMARK-NAME*)))
          (log-gtp-benchmarks-info "collecting module graph for ~a" bm)
          (define tu (benchmark->typed/untyped-dir bm))
          (define G (get-modulegraph tu))
          (void
            (clean-staging!))
          (values bm (simplify-module-names G tu)))))))

(define (format-require-typed-check-info)
  (define bm+rtc* (get-require-typed-check-info))
  (for/list ([bm+rtc (in-list bm+rtc*)])
    (list
      (subsubsection (format "~a Boundary Types" (car bm+rtc)))
      (linebreak)
      (format-rtc-info* (cdr bm+rtc)))))

(define (format-rtc-info* rtc*)
  (define seen (mutable-set))
  (for/list ((rtc (in-list rtc*))
             #:unless (set-member? seen (require-typed-check-info-src rtc)))
    (set-add! seen (require-typed-check-info-src rtc))
    (format-rtc-info rtc)))

(define (format-rtc-info rtc)
  (define importing-mod (require-typed-check-info-src rtc))
  (define sexp (require-typed-check-info-sexp rtc))
  (list
    (emph (path->string (file-name-from-path importing-mod)))
    (linebreak)
    (verbatim (pretty-format sexp))
    (linebreak)))

(define (format-types t*)
  (apply itemize (for/list ((t (in-list t*))) (item (~a t)))))

(define (get-require-typed-check-info)
  (parameterize ([*with-cache-fasl?* #false]
                 [*current-cache-directory* cache-path]
                 [*current-cache-keys* (list (lambda () benchmarks-key*))])
    (with-cache (cachefile "require-typed-check-info.rktd")
      (lambda ()
        (log-gtp-benchmarks-info "collecting type annotations (ETA 40 minutes)")
        (for/list ((bm-name (in-list BENCHMARK-NAME*)))
          (log-gtp-benchmarks-info "collecting annotations for ~a" bm-name)
          (define tu-dir (benchmark->typed/untyped-dir bm-name))
          (define rtc*
            (dynamic-wind (lambda ()
                            (setup-typed-configuration! tu-dir)
                            (clean-directory! staging-path))
                          (lambda ()
                            (compile/require-typed-check-info (build-path staging/config-path main-name)))
                          clean-staging!))
          (cons bm-name rtc*))))))

(define (get-count-chaperones-bin)
  ;; Example: "/home/ben/code/racket/6.12cc/bin/"
  (raise-user-error 'get-count-chaperones-bin "not implemented --- if you have a version of Racket with the chaperone-counting patch, please change the body of this function to point to the version's `bin/` folder"))

(define (get-count-chaperones-info)
  (parameterize ([*with-cache-fasl?* #false]
                 [*current-cache-directory* cache-path]
                 [*current-cache-keys* (list (lambda () benchmarks-key*))])
    (with-cache (cachefile "count-chaperones.rktd")
      (lambda ()
        (log-gtp-benchmarks-info "collecting chaperone counts (ETA ??? hours)")
        (for/list ((bm-name (in-list BENCHMARK-NAME*)))
          (log-gtp-benchmarks-info "collecting chaperones for ~a" bm-name)
          (define tu-dir (benchmark->typed/untyped-dir bm-name))
          (define cc-bin (get-count-chaperones-bin))
          (define cc*
            (dynamic-wind (lambda ()
                            (setup-typed-configuration! tu-dir)
                            (clean-directory! staging-path))
                          (lambda ()
                            (count-chaperones cc-bin (build-path staging/config-path main-name)))
                          clean-staging!))
          (cons bm-name cc*))))))

(define (format-chaperones-info)
  (define cc* (get-count-chaperones-info))
  (for/list ((title+key (in-list '((("Proc. apps" . proc_apps)
                                    ("Proc. makes" . proc_makes)
                                    ("Proc. depth" . proc_maxdepth))
                                   (("Struct apps" . struct_apps)
                                    ("Struct makes" . struct_makes)
                                    ("Struct depth" . struct_maxdepth))
                                   (("Vec. apps" . vec_apps)
                                    ("Vec. makes" . vec_makes)
                                    ("Vec. depth" . vec_maxdepth))))))
    (define (title->key t)
      (define v (assoc t title+key))
      (if v
        (cdr v)
        (raise-arguments-error 'format-chaperones-info "invalid title" "title" t)))
    (make-table/horizontal cc* (map car title+key) title->key add-commas)))

(define (format-time-info)
  (define cc* (get-count-chaperones-info))
  (define title+key
    '(("Milliseconds" . current-process-milliseconds)
      ("GC Milliseconds" . current-gc-milliseconds)
      ("Num. GC" . num-garbage-collections)
      ("Peak Bytes" . peak-allocated-bytes)))
  (define (title->key t)
    (define v (assoc t title+key))
    (if v
      (cdr v)
      (raise-arguments-error 'format-chaperones-info "invalid title" "title" t)))
  (make-table/horizontal cc* (map car title+key) title->key add-commas))

;; =============================================================================

(module+ test

  (test-case "maybe-rnd"
    (check-equal? (maybe-rnd 0) "0")
    (check-equal? (maybe-rnd 0.123) "0.12"))

  (test-case "benchmark->typed/untyped-dir"
    (check-equal? (benchmark->typed/untyped-dir 'sieve) (build-path benchmarks-path "sieve"))
    (check-equal? (benchmark->typed/untyped-dir 'gregor) (build-path benchmarks-path "gregor")))

  (test-case "get-untyped-loc"
    (check-equal? (get-untyped-loc (benchmark->typed/untyped-dir 'dungeon)) 541))

  (test-case "get-typed-loc"
    (check-equal? (get-typed-loc (benchmark->typed/untyped-dir 'dungeon)) 610))

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
      (check-equal? (modulegraph->num-internal-boundaries mg) expected-num-boundaries)
      (check-equal? (modulegraph->num-internal-exports mg) expected-num-exports)
      (void))
    (test-modulegraph 'sieve 2 1 9)
    (test-modulegraph 'morsecode 4 3 15)
    (test-modulegraph 'snake 8 16 31))

  (test-case "benchmark->modulegraph"
    ;; depends on cache
    (define g (benchmark->modulegraph 'mbta))
    (define tu (benchmark->typed/untyped-dir 'mbta))
    (check-equal? (modulegraph->num-modules g) 4)
    (check-equal? (modulegraph->num-internal-boundaries g) 3)
    (check-equal? (modulegraph->num-externals g) 1)
    (check-equal? (modulegraph->numbered-names g)
                  #hash(("main.rkt" . 0)
                        ("run-t.rkt" . 1)
                        ("t-graph.rkt" . 2)
                        ("t-view.rkt" . 3)
                        ("../base/my-graph.rkt" . 4)))
    (define nt (modulegraph->name-table g))
    (check-equal? nt
                  '((("main.rkt" . 0) ("t-view.rkt" . 3))
                    (("run-t.rkt" . 1) ("../base/my-graph.rkt" . 4))
                    (("t-graph.rkt" . 2) #false)))
    (check-pred values (format-module-names nt)))

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

  (test-case "rowize"
    (check-equal? (rowize '(A B C D E F) 2) '((A D) (B E) (C F)))
    (check-equal? (rowize '(A B C) 2) '((A C) (B #f))))

  (test-case "maybe-split-at"
    (let-values (((a b) (maybe-split-at '() 0)))
      (check-equal? a '())
      (check-equal? b '()))
    (let-values (((a b) (maybe-split-at '(A B C) 1)))
      (check-equal? a '(A))
      (check-equal? b '(B C)))
    (let-values (((a b) (maybe-split-at '(A B C) 4)))
      (check-equal? a '(A B C))
      (check-equal? b '())))

  (test-case "complete-path->imported-modules"
    (let ([sieve-main (build-path (benchmark->typed/untyped-dir 'sieve) untyped-name "main.rkt")]
          [sieve-streams (build-path (benchmark->typed/untyped-dir 'sieve) untyped-name "streams.rkt")])
      (check-equal? (complete-path->imported-modules sieve-main) (list sieve-streams))))

  (test-case "complete-path->exported-identifiers"
    (let* ([sieve-streams (build-path (benchmark->typed/untyped-dir 'sieve) untyped-name "streams.rkt")]
           [exp* (complete-path->exported-identifiers sieve-streams)])
      (check set=? exp*
             '(make-stream stream-first stream-get stream-rest stream-take stream-unfold stream? struct:stream stream))))

)
