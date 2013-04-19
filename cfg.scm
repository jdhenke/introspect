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
(define *base* 'base) ;dummy note for dummy source node
(define *global* 'global)
(define *normal* 'normal)

;;; Create a cfg object and return it.
(define (create-cfg) (create-graph))

;;; Simply add the function f to cfg. Returns #t on success, #f otherwise.
(define (cfg:add-function cfg f)
  ;; TODO: (assert (graph? cfg) "can only operate on graph objects!")
  ;; TODO? (assert (not (exists-node? cfg f)) "function already exists")
  (add-node cfg f))

;;; Private function: given a cfg and function, find corresponding node
(define (cfg:find-function cfg f)
  (let ((nodes (get-nodes cfg)))
    (let ((f-node (find (lambda (n) (eq? (get-node-data n) f)) nodes)))
      f-node)))

(define (cfg:add-define-edge f sub-f)
  (add-edge f sub-f *define-edge*))

(define (cfg:add-call-edge caller callee)
  (add-edge caller callee *call-edge*))

;;; given a cfg object and a function (name?) defined at the top level (not
;;; called by anyone), add the function to the cfg as a source node
(define define-global-func cfg:add-function)

;;; Given a cfg object and a parent function which makes a call to sub-f
;;; function, add the new sub-f to the cfg and add a directed edge from parent
;;; to sub-f. Use this to declare that
(define (define-sub-function cfg parent sub-f)
  (let ((p-f (cfg:find-function cfg parent))
	(sub-f (cfg:add-function cfg sub-f)))
    (cfg:add-define-edge p-f sub-f)))

;;; Add a function call to a cfg
;;; Given the cfg, calling function and the called function, add a directed edge
;;; from caller to callee.
(define (add-function-call cfg caller callee)
  (let ((cr-f (cfg:find-function cfg caller))
	(ce-f (cfg:find-function cfg callee)))
    (assert (not (eq? cr-f #f)) "caller not in cfg")
    (assert (not (eq? ce-f #f)) "callee not in cfg")
    (cfg:add-call-edge cr-f ce-f)))

;;; Given a function, return the procedure which defined it
;; TODO: should be a scope object??
(define (cfg:defined-by cfg f)
  (let* ((func (cfg:find-function cfg f))
	 (edges (get-incoming-edges func)))
    (let ((edge (find (lambda (e) (eq? (get-edge-data e) *define-edge*)) edges)))
      (assert (not (eq? edge #f)) "should have single incoming edge")
      (edge-src-node edge))))

(define (cfg:get-defines cfg f)
  (let* ((func (cfg:find-function cfg f))
	 (edges (get-outgoing-edges func)))
    (map edge-dest-node ((define-edges
			   (filter (lambda (e)
				     (eq? (get-edge-data e)
					  *define-edge*)) edges))))))

;;; Given a function, find all other procedures within the same namespace.
(define (cfg:get-peers cfg f)
  (let* ((func (cfg:find-function cfg f))
	 (parent (cfg:defined-by cfg f))
	 (peers (cfg:get-defines cfg (get-node-data parent))))
    (filter (lambda (p) (not (eq? p func))) peers)))
