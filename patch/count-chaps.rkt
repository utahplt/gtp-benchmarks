#lang typed/racket/base

;; Script for counting chaperones in an instrumented version of Racket.
;;
;; Usage:
;; - Apply count-chaps patch
;; - (require gtp-checkup/private/count-chaps)
;;   (count-chaps)

(module untyped racket/base
  (provide
    count-chaps
    (struct-out chaps))

  (require
    (only-in ffi/unsafe _array in-array _int get-ffi-obj))

  (struct chaps (
    proc_makes
    proc_apps
    proc_maxdepth

    struct_makes
    struct_apps
    struct_maxdepth

    vec_makes
    vec_apps
    vec_maxdepth
  ) #:prefab )

  ;; Fill a `chaps` struct with data from the runtime, then print it.
  (define (count-chaps [outfile #f])
    (if (path-string? outfile)
      (with-output-to-file
        outfile
        #:exists 'append
        do-count-chaps)
      (do-count-chaps)))

  (define (do-count-chaps)
    ;(let ([v (make-vector 12 #f)])
    ;  (vector-set-performance-stats! v)
    ;  (displayln ";; vector-set-performance-stats!")
    ;  (displayln v))
    (displayln ";; chaps: proc_makes proc_apps proc_maxdepth struct_makes struct_apps struct_maxdepth vec_makes vec_apps vec_maxdepth")
    (writeln
     (chaps
       ;; -- fun
       (get-ffi-obj 'proc_makes #f _int)
       (get-ffi-obj 'proc_apps #f _int)
       (get-ffi-obj 'proc_maxdepth #f _int)
       ;; -- struct
       (get-ffi-obj 'struct_makes #f _int)
       (get-ffi-obj 'struct_apps #f _int)
       (get-ffi-obj 'struct_maxdepth #f _int)
       ;; -- vector
       (get-ffi-obj 'vec_makes #f _int)
       (get-ffi-obj 'vec_apps #f _int)
       (get-ffi-obj 'vec_maxdepth #f _int))))
)

(require/typed 'untyped
  (count-chaps (-> Void)))

(provide count-chaps)
