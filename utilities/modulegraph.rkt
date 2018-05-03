#lang racket/base

;; Adjacency list to represent the modules in a directory
;;  (and their immediate dependencies)

(require racket/contract)
(provide
  complete-path->imported-modules
  complete-path->exported-identifiers

  (contract-out
    (directory->modulegraph
      (-> path-string? modulegraph?))
    (modulegraph->num-modules
      (-> modulegraph? natural?))
    (modulegraph->modules
      (-> modulegraph? list?))
    (modulegraph->num-externals
      (-> modulegraph? natural?))
    (modulegraph->externals
      (-> modulegraph? list?))
    (modulegraph->num-internal-boundaries
      (-> modulegraph? natural?))
    (modulegraph->num-internal-exports
      (-> modulegraph? natural?))
    (modulegraph->numbered-names
      (->* [modulegraph?] [#:node<? cmp/c] numbered-names/c))
    (modulegraph-tsort
      (->* [modulegraph?] [#:node<? cmp/c] (listof list?)))
    (modulegraph->direct-ancestor*
      (-> modulegraph? any/c list?))))

(require
  (only-in syntax/modresolve
    resolve-module-path)
  file/glob
  racket/path
  racket/set
  racket/math)

;; =============================================================================

(define cmp/c
  (-> any/c any/c any/c))

(define modulegraph?
  pair?)

(define numbered-names/c
  (hash/c any/c natural? #:immutable #true #:flat? #true))

(define (directory->modulegraph udir)
  (define mod* (glob (build-path udir "*.rkt")))
  (for/list ([src (in-list mod*)])
    (define dst* (complete-path->imported-modules src))
    (cons src dst*)))

(define (modulegraph->num-modules mg)
  (length (modulegraph->modules mg)))

(define (modulegraph->modules mg)
  (map car mg))

(define (modulegraph->num-externals mg)
  (length (modulegraph->externals mg)))

(define (modulegraph->externals mg)
  (define m* (modulegraph->modules mg))
  (set->list
    (for*/set ((src+dst* (in-list mg))
               (dst (in-list (cdr src+dst*)))
               #:when (not (member dst m*)))
      dst)))

(define (modulegraph->num-internal-boundaries mg)
  (define m* (modulegraph->modules mg))
  (for*/sum ([src+dst* (in-list mg)]
             [dst (in-list (cdr src+dst*))]
             #:when (member dst m*))
    1))

(define (modulegraph->num-internal-exports mg)
  (for*/sum ([src+dst* (in-list mg)])
    (length (complete-path->exported-identifiers (car src+dst*)))))

(define (modulegraph-tsort adj #:node<? [name<? string<?])
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

(define (modulegraph->numbered-names g #:node<? [name<? string<?])
  (define m* (sort (modulegraph->modules g) name<?))
  (define e* (sort (modulegraph->externals g) name<?))
  (for/hash ((k (in-list (append m* e*)))
             (v (in-naturals)))
    (values k v)))

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

;; =============================================================================

(module+ test
  (require rackunit)

  (test-case "modulegraph:synthetic"
    (define G '(("A" "B" "C")
                ("B" "C" "D")
                ("C")
                ("E")))
    (check-equal? (modulegraph->modules G) '("A" "B" "C" "E"))
    (check-equal? (modulegraph->num-internal-boundaries G) 3)
    (check-equal? (modulegraph->externals G) '("D"))
    (check-equal? (modulegraph->direct-ancestor* G "A") '())
    (check-equal? (modulegraph->direct-ancestor* G "B") '("A"))
    (check-equal? (modulegraph->direct-ancestor* G "C") '("A" "B"))
    (check-equal? (modulegraph-tsort G) '(("C" "E") ("B") ("A")))
    (define GG '(("A" "B" "C")
                 ("B" "C")))
    (check-equal? (modulegraph->externals GG) '("C")))

)
