#lang racket

(require "enode.rkt" "egraph.rkt" "ematch.rkt" "extraction.rkt")


(module+ test (require rackunit math/base))

(provide make-regraph regraph-cost regraph-count regraph-extract regraph-limit
         regraph-rinfo rinfo rinfo-match-count rinfo-search-time rinfo-rebuild-time
         rule-phase precompute-phase prune-phase extractor-phase
         regraph-eclass-count rebuild-phase regraph-rebuilding-enabled?)

(struct rinfo (match-count search-time rebuild-time) #:mutable)
(struct regraph (egraph extractor ens limit rinfo rebuilding-enabled?) #:mutable)

(define (make-rinfo)
  (rinfo 0 0 0))

(define (make-regraph exprs
                      #:limit [limit #f]
                      #:rebuilding-enabled? [rebuilding-enabled? #t])
  (define eg (mk-egraph))
  (define ens (for/list ([expr exprs]) (mk-enode-rec! eg expr)))
  (define ex (apply mk-extractor ens))
  (extractor-iterate ex)
  (regraph eg ex ens limit (make-rinfo) rebuilding-enabled?))

(define (regraph-eclass-count rg)
  (length (egraph-leaders (regraph-egraph rg))))

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

(define (find-matches ens ipats opats)
  (for ([en ens])
    (refresh-vars! en))
  (define out '())
  (for ([ipat ipats] [opat opats] #:when true [en ens])
    (define bindings (match-e ipat en))
    (unless (null? bindings)
      (set! out (cons (list* opat en bindings) out))))
  out)

(define ((rule-phase ipats opats #:match-limit [match-limit #f] #:identical-graph [id-graph #f]) rg)
  (define eg (regraph-egraph rg))
  (define limit (regraph-limit rg))
  (define search-start-time (current-inexact-milliseconds))
  (define matches (find-matches (egraph-leaders eg) ipats opats))
  (set-rinfo-search-time!
   (regraph-rinfo rg)
   (+ (rinfo-search-time (regraph-rinfo rg))
      (- (current-inexact-milliseconds) search-start-time)))
      
  
  (for* ([m matches]
         #:break (or (and limit (>= (egraph-cnt eg) limit))
                     (and match-limit (> (rinfo-match-count (regraph-rinfo rg)) match-limit))))
    (set-rinfo-match-count! (regraph-rinfo rg) (+ (rinfo-match-count (regraph-rinfo rg)) 1))
    (match-define (list opat en bindings ...) m)
    (for ([binding bindings] #:break (and limit (>= (egraph-cnt eg) limit)))
      (define expr* (substitute-e opat binding))
      (define en* (mk-enode-rec! eg expr*))
      (merge-egraph-nodes! eg en en* (regraph-rebuilding-enabled? rg)))))

(define ((precompute-phase fn) rg)
  (for ([leader (egraph-leaders eg)])
    (refresh-vars! leader))
  
  (define eg (regraph-egraph rg))
  (define limit (regraph-limit rg))
  (define to-merge
    (for/list ([en (egraph-leaders eg)]
               #:break (and limit (>= (egraph-cnt eg) limit)))
      (set-precompute! eg en fn)))
  (for ([mergelist to-merge])
    (when mergelist
      (for ([mergepair mergelist])
        (when mergepair
          (merge-egraph-nodes! eg (first mergepair) (second mergepair) (regraph-rebuilding-enabled? rg)))))))

(define ((rebuild-phase) rg)
  (define start-time (current-inexact-milliseconds))
  (egraph-rebuild (regraph-egraph rg))
  (set-rinfo-rebuild-time!
   (regraph-rinfo rg)
   (+ (rinfo-rebuild-time (regraph-rinfo rg))
      (- (current-inexact-milliseconds) start-time))))

(define (set-precompute! eg en fn)
  (for/list ([var (enode-vars en)] #:when (list? var))
    (define op (car var))
    (define args (map (lambda (e) (enode-atom (pack-leader e))) (cdr var)))
    (cond
      [(andmap identity args)
       (define constant (apply fn op args))
       (if constant
           (list en(mk-enode-rec! eg constant))
           #f)]
      [else #f])))
       

(define (prune-phase rg)
  (define eg (regraph-egraph rg))
  (define limit (regraph-limit rg))
  (for ([en (egraph-leaders eg)] #:break (and limit (>= (egraph-cnt eg) limit)))
    (reduce-to-single! eg en (regraph-rebuilding-enabled? rg))))

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
   `(6 6)))

