#lang racket

(require "../main.rkt")
(require "../egraph.rkt")
(require "./herbie/src/programs.rkt")
(require (except-in profile profile))
(require profile/render-text)
(require profile/analyzer)


(provide iteration-options)

(define iteration-options '(5000))

(define rules-exprs-port (open-input-file "./rules.txt"))
(define rules-in (read rules-exprs-port))
(define rules-out (read rules-exprs-port))

(define (read-lines port)
  (define line (read port))
  (if (equal? line eof)
      empty
      (cons line (read-lines port))))

(define (spawn-all-regraphs port node-limit rebuilding?)
  (for/list ([batch (read-lines port)])
    (make-regraph batch #:limit node-limit #:rebuilding-enabled? rebuilding?)))


(define (run-regraph regraph limits match-count-port)
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
      
      (if
       (and
        (< initial-cnt (regraph-count regraph))
        (or (not (regraph-limit regraph)) (< (regraph-count regraph) (regraph-limit regraph)))
        (or (not match-limit) (<= (rinfo-match-count (regraph-rinfo regraph)) match-limit)))
       #f
       i)))
  
  (printf "Last iteration: ~a\n" last-i)
  (list (- (rinfo-match-count (regraph-rinfo regraph)) 1)
        (+ last-i 1)))

(define (render-regraph-info-with-port all-regraphs port data)
  (fprintf port "~a\n"
           (string-join (map (λ (ele) (~a (exact->inexact (/ ele (length all-regraphs))))) data) ",")))

(define (render-csv-line file-port data)
  (fprintf file-port "~a\n"
           (string-join (map ~a data) ",")))

(define (time-suite-with exprs-filename info-port match-count-port
                         node-limit rebuild-limits)
  (fprintf (current-output-port) "\nTiming file ~a~a\n" exprs-filename
           (if rebuild-limits " with rebuilding" ""))

  (printf "Timing with node limit: ~a\n" (number->string node-limit))

  (define exprs-port (open-input-file (build-path (current-directory) exprs-filename)))

  (define all-regraphs
    (spawn-all-regraphs exprs-port (and (not rebuild-limits) node-limit)
                        (if rebuild-limits true false)))

  (define limits-iter (if rebuild-limits rebuild-limits (in-producer (lambda () false))))
  
  (for/list ([regraph all-regraphs] [i (length all-regraphs)] [limit limits-iter])
    (fprintf (current-output-port)
             "Regraph ~a\n" i)
    (flush-output)

    (define begin-time (current-inexact-milliseconds))
    (define begin-merge merge-time)
    (define begin-rebuild (rinfo-rebuild-time (regraph-rinfo regraph)))
    (define begin-find-matches (rinfo-search-time (regraph-rinfo  regraph)))
    (define new-limit
      (run-regraph regraph limit match-count-port))
    (define after (current-inexact-milliseconds))
    (define data
      (list
       (file-name-from-path exprs-filename)
       (- after begin-time)
       (- merge-time begin-merge)
       (- (rinfo-rebuild-time (regraph-rinfo regraph)) begin-rebuild)
       (- (rinfo-search-time (regraph-rinfo regraph)) begin-find-matches)))
    (render-csv-line info-port data)
    new-limit))

(define (time-suite expr-file upwards-port rebuilding-port
                    rmatch-count-port umatch-count-port)
  (for ([node-limit (in-list iteration-options)])
    (define limits (time-suite-with expr-file upwards-port rmatch-count-port node-limit false))
    (time-suite-with expr-file rebuilding-port umatch-count-port node-limit limits)))


(module+ main
  (define rebuilding? #f)
  (command-line 
   #:program "time-rebuilding"
   #:args (upwards-file rebuilding-file umatch-file rmatch-file . expr-files)
   (define upwards-port (open-output-file upwards-file #:exists 'replace))
   (define rebuilding-port (open-output-file rebuilding-file #:exists 'replace))
   (define umatch-count-port (open-output-file umatch-file #:exists 'replace))
   (define rmatch-count-port (open-output-file rmatch-file #:exists 'replace))
   (for ([expr-file expr-files])
     (printf "#########################\nTiming file: ~a\n" expr-file)
     (time-suite expr-file upwards-port rebuilding-port
                 rmatch-count-port umatch-count-port))))
