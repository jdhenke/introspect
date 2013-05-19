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
;;;   tail? - boolean value if the expression being evaluated is in tail position.
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
	     (analyze-any-application exp parent-node))
	    (else
	     (error "Unknown expression type"
		    exp))))))

(define (analyze-self-evaluating exp parent-node)
  (let ((return (default-cell exp)))
    (lambda (env tail?)
      (if tail? (apply-tags return))
      return)))

(defhandler analyze analyze-self-evaluating self-evaluating? any?)

(define (analyze-quoted exp parent-node)
  (let ((qval (default-cell (text-of-quotation exp))))
    (lambda (env tail?)
      (if tail? (apply-tags qval))
      qval)))

(defhandler analyze analyze-quoted quoted? any?)

(define (analyze-variable exp parent-node)
  (lambda (env tail?)
    (let ((return (get-variable-cell exp env)))
      (if tail? (apply-tags return))
      return)))

(defhandler analyze analyze-variable variable? any?)

(define (analyze-if exp parent-node)
  (let ((pproc (analyze (if-predicate exp) parent-node))
	(cproc (analyze (if-consequent exp) parent-node))
	(aproc (analyze (if-alternative exp) parent-node)))
    (lambda (env tail?)
      (if (true? (pproc env #f)) (cproc env tail?) (aproc env tail?)))))

(defhandler analyze analyze-if if? any?)

(define (analyze-lambda exp parent-node)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze (lambda-body exp) parent-node)))
    (lambda (env tail?)
      (default-cell (make-compound-procedure vars bproc env)))))

(defhandler analyze analyze-lambda lambda? any?)

(define (analyze-any-application exp parent-node)
  (let ((destination-name (operator exp)))
    ;; add a call edge
    (if *verbose* (begin (pp "Adding edge to/from") (pp exp) (pp parent-node)))
    (let ((edge (if (rootnode? parent-node)
		    (add-global-call *g* destination-name)
		    (add-function-call *g* parent-node destination-name)))
	  (combinator (analyze-application exp parent-node)))
      (lambda (env tail?)
	(let ((return-value (combinator env tail?)))
	  (if *verbose* (begin (pp "Adding execution") (pp exp) (pp edge)
			       (pp (operands exp)) (pp return-value)))
	  (add-execution edge (operands exp) return-value)
	  return-value)))))

(define (analyze-application exp parent-node)
  (define (analyze-tmp exp)
    (analyze exp parent-node))
  (let ((fproc (analyze (operator exp) parent-node))
	(aprocs (map analyze-tmp (operands exp))))
    (lambda (env tail?)
      (let* ((proc-cell (fproc env #f))
	     (proc-tags (cell-tags proc-cell)))
	(if tail?
	    (begin (enqueue proc-tags)
		   (execute-application
		    proc-cell
		    (map (lambda (aproc) (aproc env #f)) aprocs) #t))
	    (add-cell-tags!
	     (execute-application
	      proc-cell
	      (map (lambda (aproc) (aproc env #f)) aprocs)
	      #f)
	     (cell-tags proc-cell)))))))

(defhandler analyze analyze-application escaped-apply? any?)

(define execute-application
  (make-generic-operator 3 'execute-application
    (lambda (proc-cell args-cells tail?)
      (error "Unknown procedure type" proc-cell))))

(defhandler execute-application
  (lambda (proc-cell args-cells tail?)
    (let* ((proc (cell-value proc-cell))
	   (vars (procedure-parameters proc))
	   (body (procedure-body proc))
	   (proc-env (procedure-environment proc)))
      (let ((new-env (extend-environment vars args-cells proc-env)))
	    (body new-env tail?))))
  compound-procedure?)

(defhandler execute-application
  (lambda (proc-cell args-cells tail?)
    (let ((return (apply-primitive-procedure proc-cell args-cells)))
      (if tail? (apply-tags return))
      return))
  strict-primitive-procedure?)

(define (analyze-sequence exps parent-node)
  (define (sequentially proc1 proc2)
    (lambda (env tail?) (proc1 env #f) (proc2 env #f)))
  (define (loop first-proc rest-procs)
    (cond ((null? rest-procs) first-proc)
	  ((= 1 (length rest-procs))
	   (lambda (env tail?) (proc1 env #f) ((car rest-procs) env #t)))
	  (else (loop (sequentially first-proc (car rest-procs))
		      (cdr rest-procs)))))
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
    (lambda (env tail?)
      (let ((cell (vproc env #f)))
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
      (lambda (env tail?)
	(let ((cell (vproc env #f)))
	  (define-variable! var cell env)
	  (default-cell 'ok))))))

(defhandler analyze analyze-definition definition? any?)

;;; Macros (definitions are in syntax.scm)

(defhandler analyze (lambda (exp node) (analyze (cond->if exp) node))
  cond? any?)

(defhandler analyze (lambda (exp node) (analyze (let->combination exp) node))
  let? any?)

;;; Special forms to get tags
(define (analyze-get-tags exp parent-node)
  (begin
    (if *verbose* (begin (pp "analyze-get-tags") (pp exp)))
    (let ((var-obj (analyze (tag-var exp) parent-node)))
      (lambda (env tail?)
	(let* ((cell (var-obj env #f))
	       (return (make-cell (cell-tags cell) (cell-tags cell))))
	  (if tail? (apply-tags return))
	  return)))))

(define (analyze-add-tag exp parent-node)
  (begin
    (if *verbose* (begin (pp "analyze-add-tag") (pp exp)))
    (let ((aobj (analyze (tag-var exp) parent-node))
	  (atag (analyze (tag-tag exp) parent-node)))
      (lambda (env tail?)
	(let* ((cell (aobj env #f))
	       (tag-cell (atag env #f))
	       (tag (cell-value tag-cell))
	       (return (add-cell-tag! cell tag)))
	  (if tail? (apply-tags return))
	  return)))))

(defhandler analyze analyze-get-tags get-tags? any?)
(defhandler analyze analyze-add-tag add-tag? any?)

(define (analyze-ignore exp parent-node)
  (lambda (env tail?)
    ;; set hook repl eval to default repl eval
    ;; call repl/eval with expression and generic-evaluation-environment
    ;; reset hook/repl-eval as in setup.scm
    (set! hook/repl-eval default-repl-eval)
    (let ((ret (default-cell (default-repl-eval (cadr exp)
		    generic-evaluation-environment 'sussman-explain-me?))))
      (set! hook/repl-eval our-repl-eval)
      (if tail? (apply-tags ret))
      ret)))

(defhandler analyze analyze-ignore
  (lambda (exp)
    (and (list? exp)
	 (eq? 'ignore (car exp)))))