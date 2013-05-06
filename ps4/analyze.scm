;;;; Separating analysis from execution.
;;;   Generic analysis, but not prepared for
;;;   extension to handle nonstrict operands.

;;; EVALUATION
;;; Takes place in two separate phases:
;;;   1) analyze compiles the expression into a combinator
;;;   2) run the combinator with the given environment to yield the answer cell
;;;
;;;   exp - raw expression read in
;;;   env - environment in which to execute this
;;;   returns - cell of answer
(define (eval exp env)
  ((analyze exp) env))

;;; ANALYZE
;;; A function which, when given an expression returns a combinator
;;; A combinator is a function which given and environment produces
;;; The result of evaluating the expression in the environment
;;; Returns the cell of the answer
(define analyze
  (make-generic-operator 1 'analyze
    (lambda (exp)
      (cond ((application? exp)
	     (analyze-application exp))
	    (else
	     (error "Unknown expression type"
		    exp))))))

(define (analyze-self-evaluating exp)
  (lambda (env) (default-cell exp)))

(defhandler analyze analyze-self-evaluating self-evaluating?)

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) (default-cell qval))))

(defhandler analyze analyze-quoted quoted?)

(define (analyze-variable exp)
  (lambda (env) (get-variable-cell exp env)))

(defhandler analyze analyze-variable variable?)

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
	(cproc (analyze (if-consequent exp)))
	(aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env)) (cproc env) (aproc env)))))

(defhandler analyze analyze-if if?)

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze (lambda-body exp))))
    (lambda (env)
      (default-cell (make-compound-procedure vars bproc env)))))

(defhandler analyze analyze-lambda lambda?)

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
    (lambda (env)
      (let ((proc-cell (fproc env)))
	(add-cell-tags!
	 (execute-application
	  proc-cell
	  (map (lambda (aproc) (aproc env)) aprocs))
	 (cell-tags proc-cell))))))

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

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (if (null? exps) (error "Empty sequence"))
  (let ((procs (map analyze exps)))
    (loop (car procs) (cdr procs))))

(defhandler analyze
  (lambda (exp)
    (analyze-sequence (begin-actions exp)))
  begin?)


(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-value exp))))
    (lambda (env)
      (let ((cell (vproc env)))
	(set-variable-cell! var cell env)
	(default-cell 'ok)))))

(defhandler analyze analyze-assignment assignment?)

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
	(vproc (analyze (definition-value exp))))
    (lambda (env)
      (let ((cell (vproc env)))
	(define-variable! var cell env)
	(default-cell 'ok)))))

(defhandler analyze analyze-definition definition?)

;;; Macros (definitions are in syntax.scm)

(defhandler analyze (compose analyze cond->if) cond?)

(defhandler analyze (compose analyze let->combination) let?)

;;; Special forms to get tags
(define (analyze-get-tags exp)
  (begin
    (pp "analyze-get-tags")
    (pp exp)
    (let ((var-exp (tag-var exp)))
      (lambda (env)
	(let ((cell ((analyze var-exp) env)))
	  (make-cell (cell-tags cell) (cell-tags cell)))))))

(define (analyze-add-tag exp)
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

(defhandler analyze analyze-get-tags get-tags?)
(defhandler analyze analyze-add-tag add-tag?)
