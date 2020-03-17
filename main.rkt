#lang racket

(require "enode.rkt" "egraph.rkt" "ematch.rkt" "extraction.rkt")

(module+ test (require rackunit math/base))

(provide make-regraph regraph-cost regraph-count regraph-extract regraph-limit
         rule-phase precompute-phase prune-phase extractor-phase find-matches-time regraph-match-count regraph-eclass-count)

(struct regraph (egraph extractor ens limit match-count) #:mutable)

(define (make-regraph exprs #:limit [limit #f])
  (define eg (mk-egraph))
  (define ens (for/list ([expr exprs]) (mk-enode-rec! eg expr)))
  (define ex (apply mk-extractor ens))
  (extractor-iterate ex)
  (regraph eg ex ens limit 0))

(define (regraph-eclass-count rg)
  (length (set->list (list->set (egraph-leaders (regraph-egraph rg))))))

(define (regraph-cost rg)
  (apply extractor-cost (regraph-extractor rg) (regraph-ens rg)))

(define (regraph-count rg)
  (egraph-cnt (regraph-egraph rg)))

(define (regraph-extract rg)
  (map cdr (apply extractor-extract (regraph-extractor rg) (regraph-ens rg))))

;; Tries to match the rules against the given enodes, and returns a
;; list of matches found. Matches are of the form:
;; 
;; (rule enode . bindings)
;;
;; where bindings is a list of different matches between the rule and
;; the enode.

(define find-matches-time 0)
(define (find-matches ens ipats opats)
  (define begin-time (current-inexact-milliseconds))
  (define out '())
  (for ([ipat ipats] [opat opats] #:when true [en ens])
    (define bindings (match-e ipat en))
    (unless (null? bindings)
      (set! out (cons (list* opat en bindings) out))))
  (set! find-matches-time
        (+ find-matches-time (- (current-inexact-milliseconds) begin-time)))
  out)

(define ((rule-phase ipats opats #:match-limit [match-limit #f]) rg)
  (define eg (regraph-egraph rg))
  (define limit (regraph-limit rg))
  (for* ([m (find-matches (egraph-leaders eg) ipats opats)]
         #:break (or (and limit (>= (egraph-cnt eg) limit))
                     (and match-limit (> (regraph-match-count rg) match-limit))))
    (set-regraph-match-count! rg (+ (regraph-match-count rg) 1))
    (match-define (list opat en bindings ...) m)
    (for ([binding bindings] #:break (and limit (>= (egraph-cnt eg) limit)))
      (define expr* (substitute-e opat binding))
      (define en* (mk-enode-rec! eg expr*))
      (merge-egraph-nodes! eg en en*))))

(define ((precompute-phase fn) rg)
  (define eg (regraph-egraph rg))
  (define limit (regraph-limit rg))
  (for ([en (egraph-leaders eg)]
        #:break (and limit (>= (egraph-cnt eg) limit)))
    (set-precompute! eg en fn)))

(define (set-precompute! eg en fn)
  (for ([var (enode-vars en)] #:when (list? var))
    (define op (car var))
    (define args (map enode-atom (cdr var)))
    (when (andmap identity args)
      (define constant (apply fn op args))
      (when constant
        (define en* (mk-enode-rec! eg constant))
        (merge-egraph-nodes! eg en en*)))))

(define (prune-phase rg)
  (unless #t ;; PRUNING DISABLED
    (define eg (regraph-egraph rg))
    (define limit (regraph-limit rg))
    (for ([en (egraph-leaders eg)] #:break (and limit (>= (egraph-cnt eg) limit)))
      (reduce-to-single! eg en))))

(define (extractor-phase rg)
  (extractor-iterate (regraph-extractor rg)))

(module+ test
  (define (test-in-graph exprs rule-pairs check)
    (define in-rules
      (for/list ([p rule-pairs])
        (first p)))
    (define out-rules
      (for/list ([p rule-pairs])
        (second p)))
    (define regraph (make-regraph exprs #:limit 100))
    ((rule-phase in-rules out-rules) regraph)
    (extractor-phase regraph)
    (check-equal? (regraph-extract regraph) check))

  (define basic-rules
     `([(+ x 0), `x]))

  
  (test-in-graph `((+ a 0)) basic-rules `(a))


  ;; test that upward merging did its job
  (define upwards-rules
    `([(a 1 2) 3]
      [(b (a 1 2) 4) 6]))

  
  (test-in-graph
   `((b (a 1 2) 4))
   upwards-rules
   `(6))
  
  (test-in-graph
   `((b 3 4))
   upwards-rules
   `((b 3 4)))
  (test-in-graph
   `((b 3 4) (b (a 1 2) 4))
   upwards-rules
   `(6 6))


  (define (random-expr)
    (if
     (< 1 (random-integer 0 2))
     (list (random-integer 0 4) (random-expr) (random-expr))
     (random-integer 0 80)))
  (define random-rules-in
    (for/list ([i (range 5)])
      (random-expr)))
  (define random-rules-out
    (for/list ([i (range 5)])
      (random-expr)))
  (define random-exprs
    (for/list ([i (range 100000)])
      (random-expr))))

