;;;; Separating analysis from execution.
;;;   Generic analysis, but not prepared for
;;;   extension to handle nonstrict operands.

(define *g* (create-cfg))
(define rootnode 'rootnode)
(define (rootnode? r) (eq? r rootnode))

;;; EVALUATION
;;; Takes place in two separate phases:
;;;   1) analyze compiles the expression into a combinator
;;;   2) run the combinator with the given environment to yield the answer cell
;;;
;;;   exp - raw expression read in
;;;   env - environment in which to execute this
;;;   returns - cell of answer
(define (eval exp env)
  ((analyze exp rootnode) env))

;;; ANALYZE
;;; A function which, when given an expression returns a combinator
;;; A combinator is a function which given and environment produces
;;; The result of evaluating the expression in the environment
;;; Returns the cell of the answer
(define analyze
  (make-generic-operator 2 'analyze
    (lambda (exp parent-node)
      (cond ((application? exp)
	     (analyze-application exp parent-node))
	    (else
	     (error "Unknown expression type"
		    exp))))))

(define (analyze-self-evaluating exp parent-node)
  (lambda (env) (default-cell exp)))

(defhandler analyze analyze-self-evaluating self-evaluating? any?)

(define (analyze-quoted exp parent-node)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) (default-cell qval))))

(defhandler analyze analyze-quoted quoted? any?)

(define (analyze-variable exp parent-node)
  (lambda (env) (get-variable-cell exp env)))

(defhandler analyze analyze-variable variable? any?)

(define (analyze-if exp parent-node)
  (let ((pproc (analyze (if-predicate exp) parent-node))
	(cproc (analyze (if-consequent exp) parent-node))
	(aproc (analyze (if-alternative exp) parent-node)))
    (lambda (env)
      (if (true? (pproc env)) (cproc env) (aproc env)))))

(defhandler analyze analyze-if if? any?)

(define (analyze-lambda exp parent-node)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze (lambda-body exp) parent-node)))
    (lambda (env)
      (default-cell (make-compound-procedure vars bproc env)))))

(defhandler analyze analyze-lambda lambda? any?)

(define (analyze-application exp parent-node)
  (define (analyze-tmp exp)
    (analyze exp parent-node))
  (let ((destination-name (operator exp)))
    ;; add a call edge
    (pp "Adding edge to/from")
    (pp exp)
    (pp parent-node)
    (pp (string (operator exp)))
    (if (rootnode? parent-node)
	     (add-global-call *g* destination-name)
	     (add-function-call *g* parent-node destination-name))
    (let ((fproc (analyze (operator exp) parent-node))
	  (aprocs (map analyze-tmp (operands exp))))
      (lambda (env)
	(let ((proc-cell (fproc env)))
	  (add-cell-tags!
	   (execute-application
	    proc-cell
	    (map (lambda (aproc) (aproc env)) aprocs))
	   (cell-tags proc-cell)))))))

(define execute-application
  (make-generic-operator 2 'execute-application
    (lambda (proc-cell args-cells)
      (error "Unknown procedure type" proc-cell))))

(defhandler execute-application
  (lambda (proc-cell args-cells)
    (let* ((proc (cell-value proc-cell))
	   (vars (procedure-parameters proc))
	   (body (procedure-body proc))
	   (proc-env (procedure-environment proc)))
      (let ((new-env (extend-environment vars args-cells proc-env)))
	    (body new-env))))
  compound-procedure?)

(defhandler execute-application
  apply-primitive-procedure
  strict-primitive-procedure?)

(define (analyze-sequence exps parent-node)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (if (null? exps) (error "Empty sequence"))
  (define (analyze-tmp exp)
    (analyze exp parent-node))
  (let ((procs (map analyze-tmp exps)))
    (loop (car procs) (cdr procs))))

(defhandler analyze
  (lambda (exp parent-node)
    (analyze-sequence (begin-actions exp) parent-node))
  begin? any?)


(define (analyze-assignment exp parent-node)
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-value exp) parent-node)))
    (lambda (env)
      (let ((cell (vproc env)))
	(set-variable-cell! var cell env)
	(default-cell 'ok)))))

(defhandler analyze analyze-assignment assignment? any?)

(define (analyze-definition exp parent-node)
  (let ((this-node
	 (if (rootnode? parent-node)
	     (define-global-func *g* (definition-variable exp))
	     (define-sub-function *g* parent-node (definition-variable exp)))))
    (let ((var (definition-variable exp))
	  (vproc (analyze (definition-value exp) this-node)))
      (lambda (env)
	(let ((cell (vproc env)))
	  (define-variable! var cell env)
	  (default-cell 'ok))))))

(defhandler analyze analyze-definition definition? any?)

;;; Macros (definitions are in syntax.scm)

(defhandler analyze (compose analyze cond->if) cond? any?)

(defhandler analyze (compose analyze let->combination) let? any?)

;;; Special forms to get tags
(define (analyze-get-tags exp parent-node)
  (begin
    (pp "analyze-get-tags")
    (pp exp)
    (let ((var-exp (tag-var exp)))
      (lambda (env)
	(let ((cell ((analyze var-exp) env)))
	  (make-cell (cell-tags cell) (cell-tags cell)))))))

(define (analyze-add-tag exp parent-node)
  (begin
    (pp "analyze-add-tag")
    (pp exp)
    (let ((aobj (analyze (tag-var exp)))
	  (atag (analyze (tag-tag exp))))
      (lambda (env)
	(let* ((cell (aobj env))
	       (tag-cell (atag env))
	       (tag (cell-value tag-cell)))
	  (add-cell-tag! cell tag))))))

(defhandler analyze analyze-get-tags get-tags? any?)
(defhandler analyze analyze-add-tag add-tag? any?)
