;;; ui.scm - our control flow graph (cfg for short) front-end

;;; Given our generic graph library, we need an interface to interact with the
;;; control flow graphs we generate for a given program. This should implement
;;; procedures to make it more natural to construct a control flow graph.

;;; Dependencies
(load "graph")
(load "utility")

;;; Define some data types that will be used
;;; These will be used to:
;;;  - differentiate between eachother
;;;  - store additional data for function calls
;;;    - these are stored as a list of executions
(define-structure function-def)
(define-structure possible-function-def)
(define-structure function-call executions)
(define-structure execution inputs output)

;;; Constants
;; descriptor type indicator
(define *desc* '*desc*)
;; function types
(define *root* 'root) ;uniqe type for the non-existant node used to unite all
		      ;global/source nodes into a peer-group
(define *global* 'global)
(define *normal* 'non-global)
(define *undefined* 'undefined)

;;; We associate a description list with each node in our cfg. These descriptors
;;; indicate which type the function is (global or not) and the name associated
;;; with it.
(define (cfg:make-descriptor type name)
  (list *desc* type name))
(define (cfg:node-type node)
  (assert (node? node) "need a node object")
  (let ((desc (get-node-data node)))
    (assert (cfg:descriptor? desc) "node data not proper description")
    (cadr desc)))
(define (cfg:node-name node)
  (assert (node? node) "need a node object")
  (let ((desc (get-node-data node)))
    (assert (cfg:descriptor? desc) "node data not proper description")
    (caddr desc)))
(define (cfg:descriptor? desc)
  (and (list? desc) (= (length desc) 3) (eq? (car desc) *desc*)))
(define (cfg:global-node? node)
  (eq? (cfg:node-type node) *global*))
(define (cfg:root-node? node)
  (eq? (cfg:node-type node) *root*))
(define (cfg? cfg)
  (and (pair? cfg) (graph? (cfg:get-graph cfg)) (node? (cfg:get-root cfg))))


;;; cfg (private) helper procedures

;;; given a node defining the local scope and a function name, find any node
;;; within the current scope called the given name. Possible returns multiple
;;; undefined nodes if they were possibly defined in different scopes but have
;;; the same name.
(define (cfg:find-defined-node p-node f)
  (define (filter-for-node nodes)
    (filter (lambda (n)
	    (eq? (cfg:node-name n) f))
	  nodes))
  (filter-for-node
   (cfg:get-nodes-by-edge p-node
			  (lambda (e)
			    (or (function-def? e)
				(possible-function-def? e))))))



;;; add 'function' of given 'type', with reference to current scope defined by
;;; p-node in the given 'cfg'
(define (cfg:add-function cfg p-node type function)
  (assert (node? p-node) "need a node object")
  ;; helper function to merge several undefined nodes into one new (defined) node.
  (define (merge-nodes nodes new-node)
    (for-each
     (lambda (node)
       (for-each
	(lambda (e)
	  (let ((etype (get-edge-data e)))
	    (cond
	     ;; remove old possible-function-def edges
	     ((possible-function-def? etype) (remove-edge! e))
	     ;; transfer call edges to new-node
	     ((function-call? etype) (set-edge-dest-node! e new-node))
	     ;; no other edge types should exist
	     (else (assert #f "shouldn't have function defs here!")))))
	  (get-incoming-edges node)))
       nodes))

  (let ((nodes (cfg:find-defined-node p-node function))
	(new-node (add-node (cfg:get-graph cfg) (cfg:make-descriptor type function))))
    (cond
     ;; no node named `function defined in this scope, add new node
     ((null? nodes)
      new-node)
     ;; replace undefined node with definition
     ((eq? (cfg:node-type (car nodes)) *undefined*)
      (begin (merge-nodes nodes new-node) new-node))
     ;; otherwise, handle case of redefining previously defined node. Do
     ;; this by removing the define edge to the old definition and creating
     ;; a new node for the new definition.
     (else (let ((def-edge (find (lambda (e)
				   (and (function-def? (get-edge-data e))
					(eq? (edge-dest-node e) nodes)))
				 (node-outgoing-edges p-node))))
	     (assert (not (eq? def-edge #f)))
	     (remove-edge! def-edge)
	     new-node)))))

;;; add an undefined function 'function' to the given 'cfg'
(define (cfg:add-undefined-function cfg caller function)
  (let ((nodes (cfg:find-defined-node caller function)))
    (if (null? nodes)
	;; If no placeholder node exists in this scope, create it
	(let ((node (add-node (cfg:get-graph cfg)
			      (cfg:make-descriptor *undefined* function))))
	  ;; recursively add possible definition edges from every ancestor to new
	  ;; node, to encode all the possible valid define edges if this undefined
	  ;; node is ever defined in the future.
	  (let lp ((parent caller))
	    (add-edge parent node (make-possible-function-def))
	    (if (cfg:root-node? parent)
		node
		(lp (cfg:defined-by parent)))))
	;; return existing one
	(begin (assert (= (length nodes) 1)) (car nodes)))))


(define (cfg:get-graph cfg)
  (car cfg))
(define (cfg:get-root cfg)
  (cadr cfg))

(define (cfg:add-define-edge f sub-f)
  (assert (node? f) "need a node object")
  (assert (node? sub-f) "need a node object")
  (let*  ((edges (get-incoming-edges sub-f))
	 (edge (find (lambda (e)
		       (function-def? (get-edge-data e)))
		     edges)))
    (assert (eq? edge #f) `(,sub-f " has already been defined"))

    (add-edge f sub-f (make-function-def))))

(define (cfg:add-call-edge caller callee)
  (assert (node? caller) "need a node object")
  (assert (node? callee) "need a node object")
  (add-edge caller callee (make-function-call '())))

;;; Given a function node, return the node for the function which defined it.
(define (cfg:defined-by func)
  (assert (node? func) "need a node object")
  (let* ((edges (get-incoming-edges func))
	 (edge (find (lambda (e)
		       (function-def? (get-edge-data e)))
		     edges)))
    (assert (not (eq? edge #f)) "should have single incoming define edge")
    (edge-src-node edge)))

;;; Given a function node, return the node of every function it defines.
(define (cfg:get-nodes-by-edge func edge-pred)
  (assert (node? func) "need a node object")
  (let ((edges (get-outgoing-edges func)))
    (map edge-dest-node
	 (filter (lambda (e) (edge-pred (get-edge-data e)))
		 edges))))

;;; Given a function node, return the node of every function it defines.
(define (cfg:get-defines func)
  (cfg:get-nodes-by-edge func function-def?))

;;; Given a function node, return the node of every function it defines.
(define (cfg:get-possible-defines func)
  (cfg:get-nodes-by-edge func possible-function-def?))

;;; Given a function node, find the nodes of all other functions within the same
;;; namespace.
(define (cfg:get-peers func)
  (assert (node? func) "need a node object")
  (let* ((parent (cfg:defined-by func))
	 (peers (cfg:get-defines parent)))
    (filter (lambda (p) (not (eq? p func))) peers)))


;;; given a node defining the local scope and a function name, find
;;; appropriate node, recursively looking upward through scopes
(define (cfg:find-callable-node p-node f)
  (define (filter-for-node nodes)
    (find (lambda (n)
	    (eq? (cfg:node-name n) f))
	  nodes))
  (let lp ((peers (cfg:get-defines p-node))
	   (parent p-node))
    (let ((func (filter-for-node peers)))
      (cond ((cfg:root-node? parent) func) ; If parent is root, return what we have
	    ((not (eq? func #f)) func) ; if we found it
	    (else (let ((parent-parent (cfg:defined-by parent)))
		    (lp (cfg:get-defines parent-parent) parent-parent)))))))

(define (cfg:add-execution edge exec)
  (assert (edge? edge))
  (assert (execution? exec))
  (let ((fc (get-edge-data edge)))
    (assert (function-call? fc))
    (set-function-call-executions!
     fc
     (append (function-call-executions fc) `(,exec)))))

;;; cfg public/functions. Everything you need to construct a proper cfg!!

;;; Create a cfg object and return it.
(define (create-cfg)
  (let ((cfg (create-graph)))
    ;;; We employ the use of a dummy root node to make our cfg a well-defined
    ;;; tree.
    (let ((root (add-node cfg (cfg:make-descriptor *root* *root*))))
      (list cfg root))))

;;; Functions are declared in one of two types of scopes: At the top level
;;; (global scope), or within the scope of another function. We need to handle
;;; these two cases separately.

;;; given a cfg object and a function name defined at the top level, add the
;;; function to the cfg as a source node. Return the newly created node.
(define (define-global-func cfg f)
  (let* ((root (cfg:get-root cfg))
	(func (cfg:add-function cfg root *global* f)))
    (cfg:add-define-edge root func)
    func))

;;; Given a cfg object, the name of a parent function and the name of the
;;; sub-function which is defined within the scope of hte parent, add the new
;;; sub-function 'sub-f' to the 'cfg' and add a directed edge from 'parent' to
;;; 'sub-f'. Return the node of the sub-function.
(define (define-sub-function cfg parent sub-f)
  (let ((sub-f (cfg:add-function cfg parent *normal* sub-f)))
    (cfg:add-define-edge parent sub-f)
    sub-f))

;;; We also care about functions which call other functions, so we add a
;;; different type of edge for invocations than we do for defines.

;;; Add a function call to a cfg
;;; Given the cfg, calling function and the called function, add a directed edge
;;; from caller to callee.
(define (add-function-call cfg caller callee)
  (let ((ce-f (cfg:find-callable-node caller callee)))
    (if (eq? ce-f #f) ; callee not defined, add place holder
	(set! ce-f (cfg:add-undefined-function cfg caller callee)))
    (cfg:add-call-edge caller ce-f)))

;;; Add an execution of the given edge with provided inputs and outputs
(define (add-execution edge inputs outputs)
  (let ((exec (make-execution inputs outputs)))
    (cfg:add-execution edge exec)
    exec))

;;; Return all executions of a given edge
(define (get-executions edge)
  (function-call-executions (get-edge-data edge)))
