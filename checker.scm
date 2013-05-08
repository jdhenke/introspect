

;;; search depth first starting from the start node. Pred is a predicate mapping
;;; from edge to a boolean value, indicating whether to traverse it or not.
(define (visit-nodes start pred)
  (define (get-edges node)
    (filter pred (node-outgoing-edges node)))
  (let visit-sub-nodes ((start start)
			(visited-nodes '()))
    (if (in? start visited-nodes)
	visited-nodes
	(let lp ((visited-nodes (append `(,start) visited-nodes))
		 (edges (get-edges start)))
	  (if (null? edges) visited-nodes
	      (lp (visit-sub-nodes (edge-dest-node (car edges)) visited-nodes)
		  (cdr edges)))))))

;;; Given a cfg, find any dead/unreachable code. Returns a list of
;;; dead/unreachable nodes. Returns an empty list if none are found.  Dead code
;;; is considered to be any sub-graph of the cfg which is not reachable from the
;;; root node by following any edges.
(define (find-dead-code cfg)
  (let ((alive-nodes (visit-nodes (cfg:get-root cfg) (lambda (n) #t)))) ;; visit all edges
    (remove (lambda (n) (in? n alive-nodes)) (graph-nodes (cfg:get-graph cfg)))))

;;; Return list of any undefined procedures in the cfg. Returns an empty list if
;;; none are found. An undefined procedure is any procedure which is called from
;;; a node defined in the graph, but there is no valid definition to be
;;; called. Put another way, this detects any calls which would result in a
;;; runtime error complaining that some procedure is not defined.
(define (find-undefined-procedures cfg)
  (filter-nodes cfg cfg:undefined-node?))

;;; Return list of nodes for any unused procedures in cfg. An unused procedure
;;; is any procedure which is defined outside of the global scope and which is
;;; never called. Since it is not in the global scope, we can conclude that it
;;; will never be called. Notice that the subtle difference between this and
;;; dead code, which can never be called/will never be executed: unused code
;;; could be called with a slight modification to the source code. Returns an
;;; empty list if all code is used.
(define (find-unused-code cfg)
  (let ((called-nodes ;; collect all nodes which are called
	 (apply append
	  (map
	   (lambda (n) (visit-nodes n (lambda (e) (function-call?
						   (get-edge-data e)))))
		   (filter-nodes cfg cfg:global-node?)))))
    (pp called-nodes)
    (remove (lambda (n) (or (cfg:root-node? n) ;; don't return root node
			    (cfg:global-node? n)
			    (cfg:primitive-node? n)
			    (in? n called-nodes)))
	    (graph-nodes (cfg:get-graph cfg)))))
