#lang racket

(require "./main.rkt")
(require "./egraph.rkt")
(require "./herbie/src/programs.rkt")
(require (except-in profile profile))
(require profile/render-text)
(require profile/analyzer)


(provide iteration-options)

(define iteration-options '(2500 5000 7500))

(define rules-exprs-port (open-input-file "./rules.txt"))
(define rules-in (read rules-exprs-port))
(define rules-out (read rules-exprs-port))

(define (read-lines port)
  (define line (read port))
  (if (equal? line eof)
      empty
      (cons line (read-lines port))))

;; return nonempty splits
(define (split-all-by l element)
  (define-values (top end) (splitf-at l (lambda (a) (not (equal? a element)))))
  (define r
    (if (<= (length end) 1)
        empty
        (split-all-by (rest end) element)))
  (if (empty? top)
      r
      (cons top r)))

(define (spawn-all-regraphs port node-limit rebuilding?)
  (for/list ([batch (split-all-by (read-lines port) "NEW BATCH")])
    (make-regraph batch #:limit node-limit #:rebuilding-enabled? rebuilding?)))


(define (run-regraph regraph limits limits-file match-count-port eclass-count-port)
  (define match-limit (if limits (first limits) #f))
  (define iter-limit (if limits (second limits) #f))
  (define last-i
    (for/or ([i (if iter-limit (range iter-limit) (in-naturals 0))])
      (define initial-cnt (regraph-count regraph))
      ((rule-phase rules-in rules-out #:match-limit match-limit) regraph)
      (when (regraph-rebuilding-enabled? regraph)
        ((rebuild-phase) regraph))
      ((precompute-phase eval-application) regraph)
      (when (regraph-rebuilding-enabled? regraph)
        ((rebuild-phase) regraph))
      (fprintf match-count-port "~a\n" (rinfo-match-count (regraph-rinfo regraph)))
      (fprintf eclass-count-port "~a\n" (regraph-eclass-count regraph))
      (if
       (and
        (< initial-cnt (regraph-count regraph))
        (or (not (regraph-limit regraph)) (< (regraph-count regraph) (regraph-limit regraph)))
        (or (not match-limit) (<= (rinfo-match-count (regraph-rinfo regraph)) match-limit)))
       #f
       i)))
  (printf "Last iteration: ~a\n" last-i)
  (fprintf limits-file "~a\n" (list (- (rinfo-match-count (regraph-rinfo regraph)) 1)
                                    (+ last-i 1))))

(define (render-regraph-info-with-port all-regraphs port data)
  (fprintf port "~a\n"
           (string-join (map (λ (ele) (~a (exact->inexact (/ ele (length all-regraphs))))) data) ",")))

(define (render-regraph-info all-regraphs time-file data)
  (render-regraph-info-with-port all-regraphs time-file data)
  (render-regraph-info-with-port all-regraphs (current-output-port) data)
  (flush-output))

(define (time-suite-with filename folder limits-folder rebuilding? [upwards-regraphs #f])
  (fprintf (current-output-port) "\nTiming file ~a~a\n" filename
           (if rebuilding? " with rebuilding" ""))
  (define average-port
    (open-output-file (build-path (current-directory) folder "averages.txt")
                      #:exists 'replace))
  (define match-count-port
    (open-output-file (build-path (current-directory) folder "match-counts-verification.txt") #:exists 'replace))

  (define eclass-count-port
    (open-output-file (build-path (current-directory) folder "eclass-counts-verification.txt") #:exists 'replace))
  
  (for ([node-limit (in-list iteration-options)])
    (define-values (suite-folder suite-file unused-flag) (split-path (string->path filename)))
    (define exprs-name (path->string (path-replace-extension suite-file "")))
    (define suite-port (open-input-file filename))
    (printf "Timing with node limit: ~a\n" (number->string node-limit))
    (define time-file (open-output-file
                       (build-path (current-directory)
                                   folder
                                   (string-append (number->string node-limit) "-"
                                                  exprs-name "-total.txt"))
                       #:exists 'replace))
    (define limits-file-name
      (build-path (current-directory) limits-folder (format "~a-~a-match-limits.txt" node-limit exprs-name)))

    (define limits-file-out
      (if rebuilding?
          (open-output-nowhere)
          (open-output-file limits-file-name #:exists 'replace)))

    (define all-regraphs
      (spawn-all-regraphs suite-port (and (not rebuilding?) node-limit) rebuilding?))

    (define limits-file
      (if rebuilding?
          (open-input-file limits-file-name)
          #f))

    (for ([regraph all-regraphs] [i (length all-regraphs)])
      (fprintf (current-output-port)
               "Regraph ~a\n" i)
      (flush-output)

      (define limits
        (if rebuilding?
            (read limits-file)
            #f))

      (define begin-time (current-inexact-milliseconds))
      (define begin-merge merge-time)
      (define begin-rebuild (rinfo-rebuild-time (regraph-rinfo regraph)))
      (define begin-find-matches (rinfo-search-time (regraph-rinfo  regraph)))
      (run-regraph regraph limits limits-file-out match-count-port eclass-count-port)
      (define after (current-inexact-milliseconds))
      (define data
        (list
         (- after begin-time)
         (- merge-time begin-merge)
         (- (rinfo-rebuild-time (regraph-rinfo regraph)) begin-rebuild)
         (- (rinfo-search-time (regraph-rinfo regraph)) begin-find-matches)))
      (render-regraph-info (list regraph) time-file
                           data))
    (close-output-port limits-file-out)))

(define (time-suite filename upwards-folder rebuild-folder limits-folder)
  (time-suite-with filename upwards-folder limits-folder false)
  (time-suite-with filename rebuild-folder limits-folder true))


(module+ main
  (define rebuilding? #f)
  (command-line 
   #:program "time-rebuilding"
   #:args (folder rebuild-folder limits-folder . expr-files)
   (for ([expr-file expr-files])
     (printf "#########################\nTiming file: ~a\n" expr-file)
     (time-suite expr-file folder rebuild-folder limits-folder))))
