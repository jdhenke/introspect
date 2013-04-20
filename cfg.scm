;;; ui.scm - our control flow graph frontend

;;; Given our graph library is generic, we need an interface to interact with
;;; the control flow graphs we generate for a given program. This should
;;; implement procedures to make it more natural to construct a control flow
;;; graph.

;;; Depend on graph api
(load "graph")
(load "utility")

;;; Constants
;; edge types
(define *define-edge* 'defines)
(define *call-edge* 'calls)
;; function scopes
(define *base* 'base) ;dummy scope for dummy source node
(define *global* 'global)
(define *normal* 'non-global)

;;; TODO: current use of "node" conflicts with graph node nomenclature!!
;;; A "node" in the cfg is a 2-element list.
;;; First element is node type, second is the function name
(define (cfg:make-node node-type node-name)
  (list node-type node-name))
(define (cfg:node-type node)
  (car node))
(define (cfg:node-name node)
  (cadr node))
(define (cfg:node? node)
  (and (list? node) (= (length node) 2)))
(define (cfg:global-node? node)
  (eq? (cfg:node-type (get-node-data node)) *global*))


;;; Simply add the function f to cfg.
(define (cfg:add-node cfg node)
  (assert (graph? cfg) "can only operate on graph objects!")
  (assert (cfg:node? node))
  ;; TODO? (assert (not (exists-node? cfg f)) "function already exists")
  (add-node cfg node))

;;; Private function: given a cfg and function, find corresponding node
(define (cfg:find-node cfg f)
  (let ((nodes (get-nodes cfg)))
    (find (lambda (n)
	    (eq? (cfg:node-name (get-node-data n)) f))
	  nodes)))

(define (cfg:add-define-edge f sub-f)
  (add-edge f sub-f *define-edge*))

(define (cfg:add-call-edge caller callee)
  (add-edge caller callee *call-edge*))


;;; Create a cfg object and return it.
(define (create-cfg)
  (let ((cfg (create-graph)))
    (cfg:add-node cfg (cfg:make-node *base* 'root))
    cfg))

;;; given a cfg object and a function (name?) defined at the top level (not
;;; called by anyone), add the function to the cfg as a source node
(define (define-global-func cfg f)
  (cfg:add-node cfg (cfg:make-node *global* f)))

;;; Given a cfg object and a parent function which makes a call to sub-f
;;; function, add the new sub-f to the cfg and add a directed edge from parent
;;; to sub-f. Use this to declare that
(define (define-sub-function cfg parent sub-f)
  (let ((p-f (cfg:find-node cfg parent))
	(sub-f (cfg:add-node cfg (cfg:make-node *normal* sub-f))))
    (cfg:add-define-edge p-f sub-f)))

;;; Add a function call to a cfg
;;; Given the cfg, calling function and the called function, add a directed edge
;;; from caller to callee.
(define (add-function-call cfg caller callee)
  (let ((cr-f (cfg:find-node cfg caller))
	(ce-f (cfg:find-node cfg callee)))
    (assert (not (eq? cr-f #f)) "caller not in cfg")
    (assert (not (eq? ce-f #f)) "callee not in cfg")
    (cfg:add-call-edge cr-f ce-f)))

;;; Given a function, return the procedure which defined it
;; TODO: should be a scope object??
(define (cfg:defined-by cfg f)
  (let ((func (cfg:find-node cfg f)))
    (if (cfg:global-node? func) #f
	(let* ((edges (get-incoming-edges func))
	       (edge (find (lambda (e)
			     (eq? (get-edge-data e) *define-edge*))
			   edges)))
	  (assert (not (eq? edge #f)) "should have single incoming edge")
	  (edge-src-node edge)))))

(define (cfg:get-defines cfg f)
  (let* ((func (cfg:find-node cfg f))
	 (edges (get-outgoing-edges func)))
    (map edge-dest-node
	 (filter (lambda (e) (eq? (get-edge-data e) *define-edge*))
		 edges))))

;;; Given a function, find all other procedures within the same namespace.
;; TODO: this doesn't work!
(define (cfg:get-peers cfg f)
  (let ((func (cfg:find-node cfg f))
	(parent (cfg:defined-by cfg f))
	(peers (cfg:get-defines cfg (get-node-data parent))))
    (filter (lambda (p) (not (eq? p func))) peers)))
