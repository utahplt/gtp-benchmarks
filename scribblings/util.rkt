#lang at-exp racket/base

(provide
  format-benchmark
  library
  bm)

(require
  (only-in scribble-abbrevs
    oxfordize
    integer->word
    format-url)
  scribble/manual
  scribble/core)

;; -----------------------------------------------------------------------------

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
    (if origin
      (list H (format-origin origin))
      '())
    (linebreak)
    (format-dependencies lib*)
    (linebreak)
    descr))

@; similar to `format-benchmark`, but might ignore more fields
(define (format-case-study #:name name
                           #:author author
                           #:purpose purpose
                           #:origin origin
                           #:depends lib*
                           . descr)
  (define H (linebreak))
  (para
    (bold name)
    (if origin
      (list H (format-origin origin))
      '())
    (linebreak)
    (format-dependencies lib*)
    (linebreak)
    descr))

(define (format-author author)
  (define more-than-one? (and (pair? author) (not (null? (cdr author)))))
  (format-key/value (format "author~a" (if more-than-one? "s" ""))
                    (if more-than-one? (oxfordize author) author)))

(define (format-purpose p)
  (format-key/value "purpose" p))

(define (format-origin str)
  (format-key/value "source" (format-url str)))

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
