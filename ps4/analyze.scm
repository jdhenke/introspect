;;;; Separating analysis from execution.
;;;   Generic analysis, but not prepared for
;;;   extension to handle nonstrict operands.

(define (eval exp env)
  ((analyze exp) env))

;;; Modified to return a value and tags
(define analyze
  (make-generic-operator 1 'analyze
    (lambda (exp)
      (cond ((application? exp)
	     (analyze-application exp))
	    (else
	     (error "Unknown expression type"
		    exp))))))

(define (analyze-self-evaluating exp)
  (lambda (env) (make-cell exp '())))

(defhandler analyze analyze-self-evaluating self-evaluating?)


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) (make-cell qval '()))))

(defhandler analyze analyze-quoted quoted?)

(define (analyze-variable exp)
  (lambda (env)
    (get-cell exp env)))

(defhandler analyze analyze-variable variable?)

;;; TODO - harder
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env)) (cproc env) (aproc env)))))

(defhandler analyze analyze-if if?)

;;; This captures the definition of a lambda expression
;;; NOTE: (define (foo <mumble>) <grumble>) reduces is turned into
;;; (define foo (lambda (<mumble>) <grumble>))

;;; TODO - harder
(define (analyze-lambda exp)
  (pp "LAMBDA DEFINED!")
  (let ((vars (lambda-parameters exp))
        (bproc (analyze (lambda-body exp))))
    ;;; wrap this procedure?
    (lambda (env)
      (make-compound-procedure vars bproc env))))

(defhandler analyze analyze-lambda lambda?)

;;; 
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (cons
       (execute-application (fproc env)
	(map (lambda (aproc) (aproc env))
	     aprocs))
       'tgif))))

(define execute-application
  (make-generic-operator 2 'execute-application
    (lambda (proc args-tags)
      (error "Unknown procedure type" proc))))

(define (flatten L)
  (let loop ((output '())
	     (L L))
    (if (null? L)
	output
	(loop (append output (car L)) (cdr L)))))

;;; TODO - references to primitive functions should be
;;; be captured I think. Would we use calls?
(defhandler execute-application
  (lambda (proc args-tags)
    (let* ((args (map car args-tags))
	   (answer (apply-primitive-procedure proc args))
	   (tags (flatten (map cdr args-tags))))
      (cons answer tags)))
  strict-primitive-procedure?)

;;; TODO - this references a call to a compound function
(defhandler execute-application
  ;;; wrap this lambda?
  (lambda (proc args-tags)
    ((procedure-body proc)
     (extend-environment 
      (procedure-parameters proc)
      args-tags
      (procedure-environment proc))))
  compound-procedure?)

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
      (let ((arg-tags (vproc env)))
	(set-variable-value! var (car arg-tags) env)
	(set-variable-tags! var (cdr arg-tags) env))
      'ok)))

(defhandler analyze analyze-assignment assignment?)

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (let ((arg-tags (vproc env)))
	(define-variable! var (car arg-tags) env)
	(set-variable-tags! var (cdr arg-tags) env))
      'ok)))

(defhandler analyze analyze-definition definition?)

;;; Macros (definitions are in syntax.scm)

(defhandler analyze (compose analyze cond->if) cond?)

(defhandler analyze (compose analyze let->combination) let?)

;;; Injecting our special case for inspection


;;; Expects a function
;;; check if is a function
(define (analyze-tag exp)
  (lambda (env)
    (let ((var (tag-var exp))
	  (tag ((analyze (tag-tag exp)) env)))
      (add-variable-tag var tag env))
    (cons 'ok, '()))) ; TODO - fix this cop out

(defhandler analyze analyze-tag tag?)

(define (analyze-get-tags exp)
  (lambda (env)
    (let ((var (tag-var exp)))
      (cons (get-variable-tags var env) '())))) ;;; TODO - fix this cop out

(defhandler analyze analyze-get-tags get-tags?)
    