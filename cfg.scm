;;; ui.scm - our control flow graph (cfg for short) front-end

;;; Given our generic graph library, we need an interface to interact with the
;;; control flow graphs we generate for a given program. This should implement
;;; procedures to make it more natural to construct a control flow graph.

;;; Dependencies
(load "graph")
(load "utility")

;;; Constants
;; edge types
(define *define-edge* 'defines)
(define *call-edge* 'calls)
;; descriptor type indicator
(define *desc* '*desc*)
;; function types
(define *root* 'root) ;uniqe type for the non-existant node used to unite all
		      ;global/source nodes into a peer-group
(define *global* 'global)
(define *normal* 'non-global)

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

;;; cfg (private) helper procedures

;;; add 'function' of given 'type' to the give 'cfg'
(define (cfg:add-function cfg type function)
  (assert (graph? cfg) "can only operate on graph objects!")
  ;; TODO? (assert (not (exists-node? cfg f)) "function already exists")
  (add-node cfg (cfg:make-descriptor type function)))

;;; given a cfg and function, find corresponding node
(define (cfg:find-node cfg f)
  (let ((nodes (get-nodes cfg)))
    (find (lambda (n)
	    (eq? (cfg:node-name n) f))
	  nodes)))

(define (cfg:add-define-edge f sub-f)
  (add-edge f sub-f *define-edge*))

(define (cfg:add-call-edge caller callee)
  (add-edge caller callee *call-edge*))

;;; Given a function, return the node for the function which defined it.
(define (cfg:defined-by cfg f)
  (let* ((func (cfg:find-node cfg f))
	(edges (get-incoming-edges func))
	(edge (find (lambda (e)
			     (eq? (get-edge-data e) *define-edge*))
			   edges)))
    (assert (not (eq? edge #f)) "should have single incoming edge")
    (edge-src-node edge)))

;;; Given a function name, return the node of every function it defines.
(define (cfg:get-defines cfg f)
  (let* ((func (cfg:find-node cfg f))
	 (edges (get-outgoing-edges func)))
    (map edge-dest-node
	 (filter (lambda (e) (eq? (get-edge-data e) *define-edge*))
		 edges))))

;;; Given a function, find the nodes of all other functions within the same
;;; namespace.
(define (cfg:get-peers cfg f)
  (let* ((func (cfg:find-node cfg f))
	(parent (cfg:defined-by cfg f))
	(peers (cfg:get-defines cfg (cfg:node-name parent))))
    (filter (lambda (p) (not (eq? p func))) peers)))


;;; cfg public/functions. Everything you need to construct a proper cfg!!

;;; Create a cfg object and return it.
(define (create-cfg)
  (let ((cfg (create-graph)))
    ;;; We employ the use of a dummy root node to make our cfg a well-defined
    ;;; tree.
    (cfg:add-function cfg *root* *root*)
    cfg))

;;; Functions are declared in one of two types of scopes: At the top level
;;; (global scope), or within the scope of another function. We need to handle
;;; these two cases separately.

;;; given a cfg object and a function name defined at the top level (not
;;; called by anyone), add the function to the cfg as a source node. Return the
;;; newly created node.
(define (define-global-func cfg f)
  (let ((root (cfg:find-node cfg *root*))
	(func (cfg:add-function cfg *global* f)))
    (cfg:add-define-edge root func)
    func))

;;; Given a cfg object, the name of a parent function and the name of the
;;; sub-function which is defined within the scope of hte parent, add the new
;;; sub-function 'sub-f' to the 'cfg' and add a directed edge from 'parent' to
;;; 'sub-f'. Return the node of the sub-function.
(define (define-sub-function cfg parent sub-f)
  (let ((p-f (cfg:find-node cfg parent))
	(sub-f (cfg:add-function cfg *normal* sub-f)))
    (cfg:add-define-edge p-f sub-f)
    sub-f))

;;; We also care about functions which call other functions, so we add a
;;; different type of edge for invocations than we do for defines.

;;; Add a function call to a cfg
;;; Given the cfg, calling function and the called function, add a directed edge
;;; from caller to callee.
(define (add-function-call cfg caller callee)
  (let ((cr-f (cfg:find-node cfg caller))
	(ce-f (cfg:find-node cfg callee)))
    (assert (not (eq? cr-f #f)) "caller not in cfg")
    (assert (not (eq? ce-f #f)) "callee not in cfg")
    (cfg:add-call-edge cr-f ce-f)))