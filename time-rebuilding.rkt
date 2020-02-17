#lang racket

(require "main.rkt")
(require "egraph.rkt")
(require (except-in profile profile))
(require profile/render-text)
(require profile/analyzer)

(define math-exprs-port (open-input-file "./math-exprs.txt"))
(define hamming-exprs-port (open-input-file "./hamming-exprs.txt"))
(define rules-exprs-port (open-input-file "./rules.txt"))
(define rules-in (read rules-exprs-port))
(define rules-out (read rules-exprs-port))

(define end #f)

(define (read-math port)
  (let ([i (read port)])
    (cond
      [(equal? eof i)
       (set! end #t)
       empty]
      [(equal? i "NEW BATCH")
       empty]
      [else
       (cons i (read-math port))])))

;; spawns next regraph or returns false if its the end of file
(define (spawn-regraph port)
  (define exprs (read-math port))
  (if (empty? exprs)
      (if end
          #f
          (make-regraph (read-math port)))
      (make-regraph exprs)))

(define (spawn-all-regraphs port)
  (set! end #f)
  (define re (spawn-regraph port))
    (if re
        (cons re (spawn-all-regraphs port))
        empty))

(define all-regraphs-math
  (spawn-all-regraphs math-exprs-port))
(define all-regraphs-hamming
  (spawn-all-regraphs hamming-exprs-port))
(println "number of regraphs math:")
(println (length all-regraphs-math))
(println "number of regraphs hamming:")
(println (length all-regraphs-hamming))

(define (run-math regraph times)
  (for ([i (range times)]
        #:break (and (regraph-limit regraph) (>= (regraph-count regraph) (regraph-limit regraph))))
    ((rule-phase rules-in rules-out) regraph)))


(define (run-all-regraphs regraphs iters)
  (for ([regraph regraphs])
    (run-math regraph iters)))



(define time-file (open-output-file "./rebuild-total.txt" #:exists 'replace))
(define merge-time-file (open-output-file "./rebuild-merge.txt" #:exists 'replace))
(define rebuild-time-file (open-output-file "./rebuild-rebuild.txt" #:exists 'replace))

(define (render-regraph-info all-regraphs data order)
  (writeln (exact->inexact (/(profile-total-time data) (length all-regraphs))) time-file)
  (for ([node (profile-nodes data)])
    (cond
      [(equal? (node-id node) 'merge-egraph-nodes!)
       (writeln (exact->inexact (/ (node-total node) (length all-regraphs))) merge-time-file)]
      [(equal? (node-id node) 'egraph-rebuild)
       (writeln (exact->inexact (/ (node-total node) (length all-regraphs))) rebuild-time-file)])))
(define NUM_ITERATIONS 4)

(println "Math timing")
(for ([i (range 4)])
  (println i)
  (profile-thunk
   (lambda () (run-all-regraphs all-regraphs-math 1))
   #:render (curry render-regraph-info all-regraphs-math)))

(println "Hamming timing")
(writeln time-file "")
(writeln merge-time-file "")
(writeln rebuild-time-file "")
(for ([i (range NUM_ITERATIONS)])
  (println i)
  (profile-thunk
   (lambda () (run-all-regraphs all-regraphs-hamming 1))
   #:render (curry render-regraph-info all-regraphs-hamming)))

