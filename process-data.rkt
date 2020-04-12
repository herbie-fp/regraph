#lang racket

(require plot/no-gui)
(require "./time-regraph.rkt")


(define folder-out (make-parameter "processed"))

(define (read-all-lines port)
  (define line (read-line port))
  (if
   (equal? line eof)
   (list)
   (cons line (read-all-lines port))))

(define (get-data data-file-path)
  (define data-port (open-input-file data-file-path))
  (define data
    (for/list ([line (read-all-lines data-port)])
      (for/list
          ([string-num (string-split (string-replace line " " "") ",")])
        (string->number string-num))))
  data)


(define (compute-averages all-data)
  (define sums
    (list
     (foldr (lambda (d r) (+ (first d) r)) 0 all-data)
     (foldr (lambda (d r) (+ (second d) r)) 0 all-data)
     (foldr (lambda (d r) (+ (third d) r)) 0 all-data)
     (foldr (lambda (d r) (+ (fourth d) r)) 0 all-data)))
  (for/list ([e sums])
    (/ e (length all-data))))


(define (process-data data filename table)
  (unless (equal? (length data) 0)
    (define elements (string-split (path->string filename) "-"))
    (unless (not (equal? (length elements) 4))
      (define num-size (string->number (first elements)))
      (define bench-name (second elements))
      (unless
          (hash-has-key?
           table bench-name)
        (hash-set! table bench-name (make-hash)))
      (hash-set! (hash-ref table bench-name)
                 num-size
                 data))))

(define (display-line data port)
  (fprintf port "~a\n" (string-join (map ~a data) ",")))


(define (make-averages-row port data-table node-limit)
  (define all-data-for-node-limit
    (for/fold
        ([acc empty])
        ([(benchname nodehash) data-table])
      (append (hash-ref nodehash node-limit) acc)))
  (display-line (cons node-limit (compute-averages all-data-for-node-limit)) port))

(define (make-benchmark-row port data-table benchmark)
  (define all-data-for-benchmark
    (hash-ref (hash-ref data-table benchmark) 5000))
  (display-line (cons benchmark (compute-averages all-data-for-benchmark)) port))

(define (start-process name)
  (define dir-name (string-append "tables-" name))
  (define data-table (make-hash))
  (for ([data-file (directory-list (build-path (current-directory)
                                               (string-append "timing-" name)))])
    (process-data
     (get-data (build-path (current-directory)
                           (string-append "timing-" name)
                           data-file))
     data-file
     data-table))

  
  (unless (directory-exists? dir-name)
    (make-directory (build-path (current-directory) dir-name)))

  (define all-averages-file
    (open-output-file (build-path (current-directory) dir-name "averages.txt")
                      #:exists 'replace))

  (define all-benchmarks-file
    (open-output-file (build-path (current-directory) dir-name "benchmarks.txt")
                      #:exists 'replace))

  (for ([node-limit (in-list iteration-options)])
    (make-averages-row all-averages-file data-table node-limit))

  (for ([benchmark (hash-keys data-table)])
    (make-benchmark-row all-benchmarks-file data-table benchmark))
  
  data-table)


(define (get-total-time nodemap)
  (define data (hash-ref nodemap 5000))
  (for/list ([r data])
    (first r)))

(define (get-search-time nodemap)
  (define data (hash-ref nodemap 5000))
  (for/list ([r data])
    (fourth r)))

(define (get-congruence-time nodemap)
  (define data (hash-ref nodemap 5000))
  (for/list ([r data])
    (+ (second r) (third r))))


(define (make-search-plot utable rtable filename label get-function filter-func)
  (define all-data-folder (build-path (current-directory) "all-data"))
  (unless (directory-exists? all-data-folder)
    (make-directory all-data-folder))
  (define all-file
    (open-output-file (build-path (current-directory) "all-data"
                                  (string-append "all-" filename  ".txt"))
                      #:exists 'replace))
  ;; keys are benchmark and values are a pair of search time points
  (define vector-table (make-hash))
  (for ([(key nodemap) utable])
    (hash-set! vector-table key
               (map vector (get-function nodemap)
                    (get-function (hash-ref rtable key)))))

  (define all-points-milliseconds
    (filter
     filter-func
     (flatten
      (for/list ([(key vectors) vector-table])
        vectors))))

  (for ([p all-points-milliseconds])
    (display-line (vector->list p) all-file))

  (define all-points
    (for/list ([vec all-points-milliseconds])
      (for/vector ([num vec])
        (/ num 1000))))

  (parameterize ([plot-x-transform  log-transform]
                 [plot-y-transform log-transform])
    (plot-file
     (list (function (λ (x) x) #:color 0 #:style 'dot)
           (points
            #:sym 'fullcircle1
            #:alpha 0.2
            all-points))
     (string-append filename ".png")
     #:x-label (string-append "upwards merging " label)
     #:y-label (string-append "rebuliding " label))))


(module+ main
  (define upwards-table (start-process "upwards"))
  (define rebuild-table (start-process "rebuilding"))

  (define (filter-low-points cutoff)
    (lambda (point) (or (< cutoff (vector-ref point 0)) (< cutoff (vector-ref point 1)))))
  (define filter-zeros
    (lambda (point) (and (> (vector-ref point 0) 0) (> (vector-ref point 1) 0))))

  (make-search-plot upwards-table rebuild-table "total-time" "total time (seconds)" get-total-time (filter-low-points 200))
  (make-search-plot upwards-table rebuild-table "search-time" "search time (seconds)" get-search-time (filter-low-points 200))
  (make-search-plot upwards-table rebuild-table "congruence-closure-time" "congruence closure time (seconds)" get-congruence-time filter-zeros)
  )
