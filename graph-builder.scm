;;; graph-builder.scm - our graph construction procedure

;;; Given a corpus of code, add nodes and edges to create a CFG
;;; for the code

;;; Dependencies
(load "ui")
(load "graph")
(load "utility")

;;; the main method
;;; example usage:
;;; (build-graph '(define (func) 3))
;;; to use:
;;; (begin
;;;   (cd "/home/dylan/Dropbox/meng/spring2013/6.945/final-introspect/ps4")
;;;   (load "load.scm")
;;;   (cd "/home/dylan/Dropbox/meng/spring2013/6.945/final-introspect")
;;;   (init))
;;; (define (r a b c) 3)

(define *g* (create-cfg))
(define rootnode (cfg:get-root *g*))

(define (build-graph code)
  (bgi code rootnode))

;; (define (build-graph-inner code parent-node)
;;   (let* ((defname (definition-variable code))
;; 	(body (definition-value code))
;; 	((defnode define-sub-function parent-node defname)))
;;     ))


;;; CODE POACHED FROM ANALYZE
;;; A function which, when given an expression returns a combinator
;;; A combinator is a function which given and environment produces
;;; The result of evaluating the expression in the environment
;;; Returns the cell of the answer
(define bgi
  (make-generic-operator 2 'build-graph-inner
    (lambda (code parent-node)
      (cond ((application? exp)
	     (begin
	       (pp "bgi-root")
	       (pp code)
	       (pp parent-node)
	       (bgi-application code parent-node)))
	    (else
	     (error "Unknown expression type"
		    exp))))))

(define (bgi-self-evaluating code parent-node)
  (begin
    (pp "bgi-self-evaluating")
    (pp code)
    (pp parent-node)
    (lambda (env) (default-cell code parent-node))))

(defhandler bgi bgi-self-evaluating self-evaluating? any?)

(define (bgi-quoted code parent-node)
  (begin
    (pp "bgi-quoted")
    (pp code)
    (pp parent-node)
    (let ((qval (text-of-quotation code)))
      (lambda (env) (default-cell qval)))))

(defhandler bgi bgi-quoted quoted? any?)

(define (bgi-variable code parent-node)
  (begin
    (pp "bgi-variable")
    (pp code)
    (pp parent-node)
    (lambda (env) (get-variable-cell exp env))))

(defhandler bgi bgi-variable variable? any?)

(define (bgi-if code parent-node)
  (begin
    (pp "bgi-if")
    (pp code)
    (pp parent-node)
    (let ((pproc (bgi (if-predicate code)))
	  (cproc (bgi (if-consequent code)))
	  (aproc (bgi (if-alternative code))))
      (lambda (env)
	(if (true? (pproc env)) (cproc env) (aproc env))))))

(defhandler bgi bgi-if if? any?)

(define (bgi-lambda code parent-node)
  (begin
    (pp "bgi-lambda")
    (pp code)
    (pp parent-node)
    (let ((vars (lambda-parameters code))
	  (bproc (bgi (lambda-body code))))
      (pp vars)
      (lambda (env)
	(default-cell (make-compound-procedure vars bproc env))))))

(defhandler bgi bgi-lambda lambda? any?)

(define (bgi-application code)
  (begin
    (pp "bgi-application")
    (pp code)
    (pp parent-node)
    (pp "Adding call edge from/to:")
    (pp code)
    (pp (string (operator code)))
    ;;(pp (string? (string (operator code))))
    (let ((fproc (bgi (operator code)))
	  (aprocs (map bgi (operands code))))
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

(define (bgi-sequence exps parent-node)
  (pp "bgi-sequence")
  (pp exps)
  (pp parent-node)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (begin
      (pp "loop")
      (pp first-proc)
      (if (null? rest-procs)
	  first-proc
	  (loop (sequentially first-proc (car rest-procs))
		(cdr rest-procs)))))
  (if (null? exps) (error "Empty sequence"))
  (let ((procs (map bgi exps)))
    (loop (car procs) (cdr procs))))

;; can trim most of above function

(defhandler bgi
  (lambda (exp)
    (bgi-sequence (begin-actions exp)))
  begin? any?)

(define (bgi-assignment code parent-node)
  (begin
    (pp "bgi-assignment")
    (pp code)
    (pp parent-node)
    (let ((var (assignment-variable code))
	  (vproc (bgi (assignment-value code))))
      (lambda (env)
	(let ((cell (vproc env)))
	  (set-variable-cell! var cell env)
	  (default-cell 'ok))))))

(defhandler bgi bgi-assignment assignment? any?)

(define (bgi-definition code parent-node)
  (begin
    (pp "bgi-definition")
    (pp code)
    (pp parent-node)
    (pp "Creating node")
    (pp (definition-variable code))
    (pp (definition-value code))
    (let ((var (definition-variable code))
	  (vproc (bgi (definition-value code))))
      (lambda (env)
	(let ((cell (vproc env)))
	  (define-variable! var cell env)
	  (default-cell 'ok))))))

(defhandler bgi bgi-definition definition? any?)

;;; Macros (definitions are in syntax.scm)

(defhandler bgi (compose bgi cond->if) cond? any?)

(defhandler bgi (compose bgi let->combination) let? any?)

