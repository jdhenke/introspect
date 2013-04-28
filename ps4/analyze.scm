;;;; Separating analysis from execution.
;;;   Generic analysis, but not prepared for
;;;   extension to handle nonstrict operands.

;;; Redefined to return a cell
(define (eval exp env)
  ((analyze exp) env))

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

;;; JDH TODO - don't want to deal wtih this case right now
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env)) (cproc env) (aproc env)))))

(defhandler analyze analyze-if if?)

;;; JDH TODO - don't want to deal with this case right now

;;; This captures the definition of a lambda expression
;;; NOTE: (define (foo <mumble>) <grumble>) reduces is turned into
;;; (define foo (lambda (<mumble>) <grumble>))
;;; 
(define (analyze-lambda exp)
  (pp "LAMBDA DEFINED!")
  (let ((vars (lambda-parameters exp))
        (bproc (analyze (lambda-body exp))))
    ;;; wrap this procedure?
    (lambda (env)
      (make-compound-procedure vars bproc env))))

(defhandler analyze analyze-lambda lambda?)

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
	(map (lambda (aproc) (aproc env))
	     aprocs)))))

(define execute-application
  (make-generic-operator 2 'execute-application
    (lambda (proc args)
      (error "Unknown procedure type" proc))))

;;; TODO - references to primitive functions should be
;;; be captured I think. Would we use calls?
(defhandler execute-application
  apply-primitive-procedure
  strict-primitive-procedure?)

;;; TODO - this references a call to a compound function
(defhandler execute-application
  ;;; wrap this lambda?
  (lambda (proc args)
    ((procedure-body proc)
     (extend-environment 
      (procedure-parameters proc)
      args
      (procedure-environment proc))))
  compound-procedure?)

;;; JDH TODO - don't feel like doing it right now
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
      (set-variable-cell! var (vproc env) env)
      (default-cell 'ok))))

(defhandler analyze analyze-assignment assignment?)

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      (default-cell 'ok))))

(defhandler analyze analyze-definition definition?)

;;; Macros (definitions are in syntax.scm)

(defhandler analyze (compose analyze cond->if) cond?)

(defhandler analyze (compose analyze let->combination) let?)
    

;;; Special sauce
(define (analyze-get-tags exp)
  (let ((var (tag-var exp)))
    (lambda (env)
      (let ((cell (get-variable-cell var env)))
	(make-cell (cell-tags cell) (cell-tags cell))))))


(define (analyze-add-tag exp)
  (let ((var (tag-var exp))
	(atag (analyze (tag-tag exp))))
    (lambda (env)
      (let* ((cell (get-variable-cell var env))
	     (tags (cell-tags cell))
	     (tag-cell (atag env))
	     (tag (cell-value tag-cell)))
	(set-cell-tags! cell (cons tag tags))
	(make-cell (cell-tags cell) (cell-tags cell))))))

(defhandler analyze analyze-get-tags get-tags?)
(defhandler analyze analyze-add-tag add-tag?)