#lang racket

(require "enode.rkt")

(provide mk-enode! mk-enode-rec! mk-egraph
	 merge-egraph-nodes!
	 egraph? egraph-cnt
	 draw-egraph egraph-leaders
         elim-enode-loops! reduce-to-single!
         merge-time egraph-rebuild
         )


;;################################################################################;;
;;# The mighty land of egraph, where the enodes reside for their entire lives.
;;# Egraphs keep track of how many enodes have been created, and keep a pointer
;;# to the top enode, defined as the enode through which all other enodes in the
;;# egraph can be reached by means of (enode-children) and (enode-expr). They
;;# also keep track of where each enode pack is referenced (expressions), and
;;# for each expression, which enode pack has it as a (enode-var).
;;#
;;# The following things should always be true of egraphs with upwards merging:
;;# 1. (egraph-cnt eg) is a positive integer.
;;# 3. For each enode en which is a key of leader->iexprs, en is the leader of
;;#    its own pack.
;;# 4. For every mapping (k, v) in leader->iexprs, for each expression e in v,
;;#    k is a member of (cdr e). That is, v is an expression that references k.
;;# 5. The set of values in leader->iexprs appended together is a subset of the
;;#    set of keys in expr->parent, when you consider enodes of the same pack
;;#    equal.
;;# 6. For every for any enode en, for every member of (enode-vars en) k, (k, v)
;;#    is in expr-parent.
;;# 7. For every mapping (k, v) in expr->parent, and every node referenced by k
;;#    is the leader of it's own pack.
;;# 8. No two enode packs have any vars in common, using the equal? definition of
;;#    equality.
;;#
;;#
;;#  Rebuilding breaks:
;;#  - Number 7
;;#  - Parent pointers in leader->iexprs. Leader->iexprs still has all the leaders as keys
;;#  - Congruence closure after merging eclasses, until the end of the rule phase when
;;#  rebuild fixes it.
;;#
;;#  Note: While the keys of leader->iexprs and the enodes referenced by the keys
;;#  of expr->parent are guaranteed to be leaders, the values of expr->parent,
;;#  and the enodes referenced by the values of leadders->iexprs are not.
;;#  This decision was made because it would require more state infrastructure
;;#  to update these values without having to make a pass over every mapping.
;;#
;;################################################################################;;

;; Only ever use leaders as keys!
(struct egraph (cnt leader->iexprs expr->parent to-merge) #:mutable)
(define merge-time 0)


(define (egraph-rebuild eg)
  (unless (equal? (egraph-rebuild-once eg) 0)
    (egraph-rebuild eg)))

(define (egraph-rebuild-once eg)
  (define to-union (mutable-set))
  (for ([sublist (egraph-to-merge eg)])
    (for ([pair sublist] #:when pair)
      (set-add! to-union (cons (pack-leader (car pair)) (pack-leader (cdr pair))))))
  (set-egraph-to-merge! eg empty)
  (for ([pair to-union])
    (merge-egraph-nodes-untimed! eg (car pair) (cdr pair) #t)
    (dedup-vars! (car pair)))
  (set-count to-union))

;; For debugging
(define (check-egraph-valid eg #:loc [location 'check-egraph-valid])
  (let ([leader->iexprs (egraph-leader->iexprs eg)]
	[count (egraph-cnt eg)])
    (unless (not (hash-has-key? leader->iexprs #f)) (error location "False is a leader!"))
    ;; The egraphs count must be a positive integer
    (unless (and (integer? count) (positive? count)) (error location "Invalid egraph count"))

    ;; Verify properties 4-6
    (for ([(leader iexprs) (in-hash leader->iexprs)])
      (unless (eq? leader (pack-leader leader)) (error location "Leader isn't a leader"))
      (unless (set-mutable? iexprs) (error location "Leader doesn't point to a mutable set"))
      (for ([iexpr iexprs])
	(unless (list? iexpr) (error location "iExpr isn't a list"))
	(unless (for/or ([sub (cdr iexpr)]) (eq? (pack-leader sub) leader))
          (error location "No subexpression of iExpr references pack"))
        (unless (for/and ([sub (cdr iexpr)]) (eq? (pack-leader sub) sub))
          (error location "Subexpression of iExpr isn't a pack leader"))
	(unless (hash-has-key? (egraph-expr->parent eg) (update-en-expr iexpr))
          (error location "iExpr isn't in expr->parent hash"))))

    ;; Verify property 7
    (for ([(k v) (in-hash (egraph-expr->parent eg))])
      ;; This line verifies that we didn't change the definition of hashing
      ;; for some part of this expression without also refreshing the binding.
      (unless (hash-has-key? (egraph-expr->parent eg) k)
        (error location "Definition of hashing expression changed without refreshing binding"))
      (when (list? k)
	(for ([en (cdr k)])
	  (unless (eq? en (pack-leader en))
            (error location "Expr in expr->parent has non-leader subexpression")))))

    ;; Verify property 8
    (let loop ([seen (set)] [rest-leaders (hash-keys leader->iexprs)])
      (let ([cur-leader-vars (enode-vars (car rest-leaders))])
	(unless (set-empty? (set-intersect (set-copy-clear seen) cur-leader-vars))
          (error location "Expectation on sets broken"))
	(when (not (null? (cdr rest-leaders)))
	  (loop (set-union cur-leader-vars seen) (cdr rest-leaders)))))))

;; Takes an egraph, and an expression, as defined in enode.rkt, and returns
;; either a new enode that has been added to the graph, mutating the state of
;; of the graph to indicate the addition, or if the expression already exists
;; in the egraph it returns the node associated with it. While the node exists
;; after this call, if we are creating a new node it still must be merged into
;; an existing node or otherwise attached to some node to be
;; completely added to the egraph.
(define (mk-enode! eg expr)
  (let ([expr* (update-en-expr expr)])
    (if (hash-has-key? (egraph-expr->parent eg) expr*)
        (let ([res (hash-ref (egraph-expr->parent eg) expr*)])
          (pack-leader res))
        (let* ([en (new-enode expr* (egraph-cnt eg))]
               [leader->iexprs (egraph-leader->iexprs eg)])
          (set-egraph-cnt! eg (add1 (egraph-cnt eg)))
          (hash-set! leader->iexprs en (mutable-set))
          (when (list? expr*)
            (for ([suben (in-list (cdr expr*))])
              (set-add! (hash-ref leader->iexprs (pack-leader suben))
                        expr*)))
          (hash-set! (egraph-expr->parent eg)
                     expr*
                     en)
          en))))

(define (mk-enode-rec! eg expr)
  (match expr
    [(list op args ...)
     (mk-enode! eg (cons op (map (curry mk-enode-rec! eg) args)))]
    [(? enode?)
     (pack-leader expr)]
    [_
     (mk-enode! eg expr)]))

;; Takes a plain mathematical expression, quoted, and returns the egraph
;; representing that expression with no expansion or saturation.
(define (mk-egraph)
  (egraph 0 (make-hash) (make-hash) (list)))

;; Gets all the pack leaders in the egraph
(define (egraph-leaders eg)
  (hash-keys (egraph-leader->iexprs eg)))

(define (dedup-vars! en)
  (update-vars! (pack-leader en) update-en-expr)
  (dedup-children! (pack-leader en)))

;; Given an egraph and two enodes present in that egraph, merge the
;; packs of those two nodes, so that those nodes return the same
;; pack-leader and enode-vars. The keys of leader->iexprs and
;; expr->parent are updated to refer to the merged leader instead of
;; the leaders of en1 and en2, but the values of those mapping are
;; not.
(define (merge-egraph-nodes! eg en1 en2 rebuilding-enabled?)
  (define begin-time (current-inexact-milliseconds))
  (begin0
      (merge-egraph-nodes-untimed! eg en1 en2 rebuilding-enabled?)
    (set! merge-time (+ merge-time (- (current-inexact-milliseconds) begin-time)))))

(define (merge-egraph-nodes-untimed! eg en1 en2 rebuilding-enabled?)    
  (match-define (egraph _ leader->iexprs expr->parent _) eg)
  ;; Operate on the pack leaders in case we were passed a non-leader
  (define l1 (pack-leader en1))
  (define l2 (pack-leader en2))

  (cond
    [(eq? l1 l2)
     ;; If the leaders are the same, then these nodes are already part
     ;; of the same pack. However, this call usually means that two
     ;; vars of this leader were found equivalent through another
     ;; merge, so we want to update the vars to remove the redundancy.
     (unless (eq? en1 en2)
       (dedup-vars! l1))]
    [else
     ;; Hold on to these vars as they won't be the same after the
     ;; merge, but we don't yet know which one we need.
     (define old-vars1 (enode-vars l1))
     (define old-vars2 (enode-vars l2))

     ;; Merge the node packs
     (define merged-en (enode-merge! l1 l2))

     ;; Now that we know which one became leader, we can bind these.
     (define-values (leader follower follower-old-vars)
       (if (eq? l1 merged-en)
           (values l1 l2 old-vars2)
           (values l2 l1 old-vars1)))

     
     ;; update expr->parent, discovering parent nodes to merge
     (define to-merge
       (update-expr->parent! eg follower-old-vars follower leader))

     ;; Now that we have extracted all the information we need from the
     ;; egraph maps in their current state, we are ready to update
     ;; them. We need to know which one is the old leader, and which is
     ;; the new to easily do this, so we branch on which one is eq? to
     ;; merged-en.
     (update-leader! eg follower-old-vars follower leader)

     ;; Now the state is consistent for this merge, so we can tackle
     ;; the other merges.
     (if rebuilding-enabled?
         (set-egraph-to-merge! eg (cons to-merge (egraph-to-merge eg)))
         (for ([node-pair (in-list to-merge)] #:when node-pair)
           (merge-egraph-nodes-untimed! eg (car node-pair) (cdr node-pair) rebuilding-enabled?)))

     (hash-remove! (egraph-leader->iexprs eg) follower)
     
     ;; The other merges can have caused new things to merge with our
     ;; merged-en from before (due to loops in the egraph), so we turn
     ;; this into a leader before finally returning it.
     (pack-leader merged-en)]))

(define (update-expr->parent! eg old-vars old-leader new-leader)
  (if (not (eq? old-leader new-leader))
      (let* ([changed-exprs (hash-ref (egraph-leader->iexprs eg) old-leader)])
        (define to-merge
          (for/list ([ch-expr (in-mutable-set changed-exprs)])
            (update-changed-expr eg ch-expr)))
        
        (for ([variation (in-set old-vars)])
          (hash-remove! (egraph-expr->parent eg) variation)
          (hash-set! (egraph-expr->parent eg)
                     (update-en-expr variation)
                     new-leader))
        to-merge)
      empty))

;; checks if we need to upwards merge two eclasses due to a changed expression
(define (update-changed-expr eg ch-expr)
  (define old-binding (hash-ref (egraph-expr->parent eg) ch-expr))
  (define replaced-iexpr (update-en-expr ch-expr))
  (define other-parent (hash-ref (egraph-expr->parent eg) replaced-iexpr #f))
  (define merge-pair
    (and other-parent
         (not (eq? (pack-leader other-parent) (pack-leader old-binding)))
         (cons other-parent old-binding)))
  
  (hash-remove! (egraph-expr->parent eg) ch-expr)
  (hash-set! (egraph-expr->parent eg) replaced-iexpr (pack-leader old-binding))
  merge-pair)

(define (update-leader! eg old-vars old-leader new-leader)
  (when (not (eq? old-leader new-leader))
    (let* ([changed-exprs (hash-ref (egraph-leader->iexprs eg) old-leader)])
      (set-union! (hash-ref! (egraph-leader->iexprs eg) new-leader (mutable-set))
                  changed-exprs)
      (for ([ch-expr (in-mutable-set changed-exprs)])
        (for ([suben (in-list (cdr ch-expr))])
          (hash-update! (egraph-leader->iexprs eg) (pack-leader suben)
                        (λ (st)
                          (for/mutable-set ([expr (in-mutable-set st)])
                            (update-en-expr expr))))))
      (hash-remove! (egraph-leader->iexprs eg) old-leader))))

;; Eliminates looping paths in the egraph that contain en. Does not
;; work if there are other looping paths.
(define (elim-enode-loops! eg en)
  (let* ([variations (enode-vars en)]
	 ;; Keep track of the expressions that could be changed by our
	 ;; actions, and the enodes that contain those expressions so
	 ;; we can update them to point to the new leader.
         [changed-exprs (hash-ref (egraph-leader->iexprs eg) (pack-leader en))]
         [changed-nodes (for/list ([expr changed-exprs])
                          (hash-ref (egraph-expr->parent eg) expr))]
	 ;; Keep track of old state so it's easier to roll back.
	 [old-expr->parent (hash-copy (egraph-expr->parent eg))]
	 [old-leader->iexprs (hash-copy (egraph-leader->iexprs eg))])

    ;; Remove all affected expressions from our expr->parent table,
    ;; since we can't remove them once we've done the pack filter,
    ;; because their hashing code changes.
    (for ([ch-en changed-nodes])
      (for-pack!
       (λ (en)
	 (hash-remove! (egraph-expr->parent eg) (enode-expr en)))
       (pack-leader ch-en)))
    ;; Remove the old node as a leader from the leader table
    (hash-remove! (egraph-leader->iexprs eg) (pack-leader en))

    ;; Filter out the first variation of the leader which loops back on
    ;; itself, by removing that enode from the pack. Keep track
    ;; of which enode we filtered.
    (define filtered #f)
    (define iexprs* '())
    (define leader*
      (pack-removef!
       (λ (en)
         (let ([loops? (and (list? (enode-expr en))
                            (for/or ([child (cdr (enode-expr en))])
                              (enode-subexpr? 1 en child)))])
           (when loops?
             (set! filtered en))
           loops?))
       (pack-leader en)))

    ;; If we didn't find any loops, skip the rest, and rollback our hash-remove!.
    (if (not filtered)
	(begin (set-egraph-expr->parent! eg old-expr->parent)
	       (set-egraph-leader->iexprs! eg old-leader->iexprs)
	       #f)
	(begin
	  (for ([suben (cdr (enode-expr filtered))])
	    (when (hash-has-key? (egraph-leader->iexprs eg) suben)
	      (set-subtract!
	       (hash-ref (egraph-leader->iexprs eg) suben)
	       (set (enode-expr filtered)))))

	  ;; Update anywhere the old enode appeared as a parent in
	  ;; expr->parent to point to the new leader.
	  (for ([expr variations])
	    (hash-set! (egraph-expr->parent eg) expr leader*))

	  ;; Loop through the expressions which contained our old leader, to update to new leader.
	  (for ([ch-en changed-nodes])
	    (for-pack!
	     (λ (en)
	       (let ([expr (enode-expr en)])
		 (when (list? expr)
		   (let ([expr* (cons (car expr)
				      (for/list ([child (cdr expr)])
					(if (equal? child filtered) leader* child)))])
		     ;; Keep track of all containing expressions.
		     (set! iexprs* (cons expr* iexprs*))
		     ;; Point them at the new leader, and also update
		     ;; their bodies if they contained the old leader.
		     (hash-set! (egraph-expr->parent eg) expr* en)
		     ;; Update the involved expressions set of all the
		     ;; children of this expression
		     (for ([child (cdr expr*)])
		       (hash-set! (egraph-leader->iexprs eg) (pack-leader child) (mutable-set expr*)))
		     (set-enode-expr! en expr*)))))
	     (pack-leader ch-en)))
	  ;; Update the set of involved expressions
	  (hash-set! (egraph-leader->iexprs eg) leader* (list->mutable-set iexprs*))
	  ;; Decrease the egraph count to account for the nodes that were removed
	  ;; Verify
	  (check-egraph-valid eg #:loc 'elimed-loops)
	  #t))))

;; If there are any variations of this enode that are a single
;; constant or variable, prune to that.
(define (reduce-to-single! eg en rebuilding-enabled?)
  (when (enode-atom en)
    (define leader (pack-leader en))
    (define old-vars
      (for/mutable-set ([var (in-set (enode-vars leader))])
          (update-en-expr var)))
    (define leader*
      (pack-filter! (λ (inner-en)
                      (not (list? (enode-expr inner-en))))
                    leader))
    (when (and (not rebuilding-enabled?) (not (eq? leader leader*)))
      (update-expr->parent! eg old-vars leader leader*)
      (update-leader! eg old-vars leader leader*))))

;; Draws a representation of the egraph to the output file specified
;; in the DOT format.
(define (draw-egraph eg fp)
  (with-output-to-file
      fp #:exists 'replace
      (λ ()
	(printf "digraph {\n")
        (printf "compound=true;\n")
	(for ([en (egraph-leaders eg)])
          (define id (enode-pid en))

          (printf "subgraph cluster_~a { \n" id)
          (printf "  style=filled; color=lightgray;\n")
          (for ([varen (pack-members en)] [vid (in-naturals)])
            (define var (enode-expr varen))
	    (printf "  ~a.~a[label=\"~a\"]\n"
		    id vid (if (list? var) (car var) var)))
          (printf "}\n"))
        (for ([en (egraph-leaders eg)])
	  (for ([varen (pack-members en)] [vid (in-naturals)] #:when (list? (enode-expr varen)))
            (define var (enode-expr varen))
            (define n (length (cdr var)))
            (for ([arg (cdr var)] [i (in-naturals)])
	      (printf "~a.~a -> ~a.0 [lhead=cluster_~a, tailport=~a]\n"
                      (enode-pid en) vid (enode-pid arg) (enode-pid (pack-leader arg))
                      (match* (i n)
                              [(0 2) "sw"] [(1 2) "se"]
                              [(0 3) "sw"] [(1 3) "s"] [(2 3) "se"]
                              [(_ _) "s"])))))
        (printf "}\n")))
  (system (format "dot -Tpng -o ~a.png ~a" fp fp)))
