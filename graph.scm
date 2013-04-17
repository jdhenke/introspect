;;; graph.scm - our directed graph sub-system used to store data

;;; JDH - a main goal is to separate external interface 
;;;       from internal implementation details.
;;;       Therefore: objects created with this API should only be operated on
;;;                  by functions within this API
;;; Basically, this is an ADT

;;; Organized by CRUD operations

;;; Create

;;; Returns a directed graph, which may be acted upon by this API
(define (create-graph)
  (error "NOT IMPLEMENTED"))

;;; Adds a new node to the graph
;;; Returns the newly created node
;;; Can optionally specify an arbitrary data object 
;;; to be attached to this node
(define (add-node graph #!optional node-data)
  (error "NOT IMPLEMENTED"))

;;; Adds a new edge to the graph; returns constructed edge
;;; The graph represents a directed edge from `from-node` to `to-node`
;;; Can optionally specify an arbitrary data object
;;; to be attached to this edge
;;; NOTE: can create multiple edges between the same two nodes
(define (add-edge graph from-node to-node #!optional edge-data)
  (error "NOT IMPLEMENTED"))

;;; Read

;;; Returns true if an edge exists from `from-node` to `to-node`
(define (edge-exists? graph from-node to-node)
  (error "NOT IMPLEMENTED"))

;;; Gets the data object attached to `node`
;;; returns `#f` if nothing has been attached
(define (get-node-data graph node)
  (error "NOT IMPLEMENTED"))

;;; Gets the data object attached to `edge`
;;; returns `#f` if nothing has been attached
(define (get-edge-data graph edge)
  (error "NOT IMPLEMENTED"))

;;; Returns list of incoming edges to node
(define (get-incoming-edges graph node)
  (error "NOT IMPLEMENTED"))

;;; Returns list of outgoing edges from node
(define (get-outgoing-edges graph node)
  (error "NOT IMPLEMENTED"))

;;; Returns all the nodes in the graph
(define (get-nodes graph)
  (error "NOT IMPLEMENTED"))

;;; Update

;;; Sets's node's data
(define (set-node-data! graph node data)
  (error "NOT IMPLEMENTED"))

;;; Sets edge's data
(define (set-edge-data! graph edge data)
  (error "NOT IMPLEMENTED"))

;;; Delete

;;; Removes node from graph
;;; Also removes any edges to or from it
(define (remove-node! graph node)
  (error "NOT IMPLEMENTED"))

;;; Removes edge from graph
;;; Leaves the nodes
(define (remove-edge! graph from-node to-node)
  (error "NOT IMPLEMENTED"))

