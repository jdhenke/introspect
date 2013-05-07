;;; graph.scm - our directed graph sub-system used to store data

;;; JDH - a main goal is to separate external interface
;;;       from internal implementation details.
;;;       Therefore: objects created with this API should only be operated on
;;;                  by functions within this API
;;; Basically, this is an ADT

;;; Internal Representation of objects

(define-structure graph nodes)
(define-structure node incoming-edges outgoing-edges data)
(define-structure edge src-node dest-node data)

;;; is this separation useful?
(define (graph-add-node graph node)
  (set-graph-nodes! graph (append (graph-nodes graph) `(,node))))

(define (node-add-incoming-edge node edge)
  ; TODO: check for double adding edge? these should be sets. implement sets?
  (set-node-incoming-edges! node (append (node-incoming-edges node) `(,edge))))

(define (node-add-outgoing-edge node edge)
  ; TODO: check for double adding edge? these should be sets. implement sets?
  (set-node-outgoing-edges! node (append (node-outgoing-edges node) `(,edge))))


;;; Organized by CRUD operations

;;; Create

;;; Returns a directed graph, which may be acted upon by this API
(define (create-graph)
  (make-graph '()))

;;; Adds a new node to the graph
;;; Returns the newly created node
;;; Can optionally specify an arbitrary data object
;;; to be attached to this node
(define (add-node graph #!optional node-data)
  (let ((node (make-node '() '() node-data)))
    (graph-add-node graph node)
    node))

;;; Adds a new edge to the graph; returns constructed edge
;;; The graph represents a directed edge from `from-node` to `to-node`
;;; Can optionally specify an arbitrary data object
;;; to be attached to this edge
;;; NOTE: can create multiple edges between the same two nodes
(define (add-edge from-node to-node #!optional edge-data)
  (let ((edge (make-edge from-node to-node edge-data)))
    (node-add-outgoing-edge from-node edge)
    (node-add-incoming-edge to-node edge)
    edge))

;;; Read

;;; Returns true if a node exists in graph
(define (exists-node? g n)
  (let loop ((nodes (get-nodes g)))
    (cond ((null? nodes) #f)
	  ((eq? n (car nodes)) #t)
	  (else (loop (cdr nodes))))))

;;; Returns true if an edge exists from `from-node` to `to-node`
(define (exists-edge? from-node to-node)
  (there-exists? (node-outgoing-edges from-node)
	    (lambda (n)
	      (eq? (edge-dest-node n) to-node))))

;;; Gets the data object attached to `node`
;;; returns `#f` if nothing has been attached
(define (get-node-data node)
  (node-data node))

;;; Gets the data object attached to `edge`
;;; returns `#f` if nothing has been attached
(define (get-edge-data edge)
  (edge-data edge))

;;; Returns list of incoming edges to node
(define (get-incoming-edges node)
  (node-incoming-edges node))

;;; Returns list of outgoing edges from node
(define (get-outgoing-edges node)
  (node-outgoing-edges node))

;;; Returns all the nodes in the graph
(define (get-nodes graph)
  (graph-nodes graph))

;;; Update

;;; set-node-data! and set-edge-data! are automagically created by scheme!

;;; Delete

;;; Removes node from graph
;;; Also removes any edges to or from it
(define (remove-node! graph node)
  (set-graph-nodes! graph
		    (delq node (graph-nodes graph)))
  (for-each (lambda (e)
		(remove-edge! e))
	    (node-outgoing-edges node))

  (for-each (lambda (e)
	      (remove-edge! e))
	    (node-incoming-edges node)))

;;; Removes edge from graph
;;; Leaves the nodes
(define (remove-edge! edge)
  (let ((src-node (edge-src-node edge))
	(dest-node (edge-dest-node edge)))
    (set-node-outgoing-edges! src-node (delq edge (node-outgoing-edges src-node)))
    (set-node-incoming-edges! dest-node (delq edge (node-incoming-edges dest-node)))))

;;; Prints all nodes in a graph
(define (pp-graph graph)
  (define (loop first-node rest-nodes)
    (if (null? rest-nodes)
	(pp first-node)
	(begin
	  (pp first-node)
	  (loop (car rest-nodes) (cdr rest-nodes)))))
  (let ((nodes (get-nodes graph)))
    (loop (car nodes) (cdr nodes))))
