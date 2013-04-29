;;; -*- Mode:Scheme -*- 

(declare (usual-integrations))

;;; Structure to abstract the idea of a scheme object from out guest interpreter
;;; Allows us to add any information we want to any object
(define-structure cell value tags)

;;; Creates a cell if only the value is known.
;;; This allows a single place to change conventions if other
;;; metadata is unspecified about the object.
(define (default-cell value)
  (make-cell value '()))


;;; Note - Predicates will receive cell as arguments
;;;        NOT values

(define the-unspecified-value (list 'the-unspecified-value))

(define (true? cell)
  (if (cell-value cell) true false))

(define (false? cell)
  (if (cell-value? cell) false true))

;;; Primitive procedures are inherited from Scheme.

(define (strict-primitive-procedure? cell)
  (procedure? (cell-value cell)))

(define (apply-primitive-procedure proc-cell args-cells)
  (let* ((proc (cell-value proc-cell))
	 (value (apply proc (map cell-value args-cells)))
	 (all-tags (apply append (map cell-tags args-cells))))
    (let loop ((all-tags all-tags)
	       (tags '()))
      (if (null? all-tags)
	  (make-cell value tags)
	  (let inner-loop ((tags-left tags))
	    (if (null? tags-left)
		(loop (cdr all-tags)
		      (cons (car all-tags) tags))
		(if (eq? (car all-tags) (car tags-left))
		    (loop (cdr all-tags)
			  tags)
		    (inner-loop (cdr tags-left)))))))))

;;; Compound procedures

(define (make-compound-procedure vars bproc env)
  (vector 'compound-procedure vars bproc env))

(define (compound-procedure? cell)
  (let ((obj (cell-value cell)))
    (vector-compound-procedure? obj)))
  
;;; Introduced this becuase the REPL loop uses this for pretty printing
(define (vector-compound-procedure? obj)
    (and (vector? obj)
	 (eq? (vector-ref obj 0) 'compound-procedure)))

(define (procedure-parameters p) (vector-ref p 1))
(define (procedure-body p) (vector-ref p 2))
(define (procedure-environment p) (vector-ref p 3))

;;; An ENVIRONMENT is a chain of FRAMES, made of vectors.
;;; TODO? - Create better abstraction of an environment?

(define (extend-environment variables cells base-environment)
  (if (fix:= (length variables) (length cells))
      (vector variables cells base-environment)
      (if (fix:< (length variables) (length cells))
	  (error "Too many arguments supplied" variables cells)
	  (error "Too few arguments supplied" variables cells))))

;;; These are useful in maintaing the abstraction of an environment

(define (environment-variables env) (vector-ref env 0))
(define (environment-cells env) (vector-ref env 1))
(define (environment-parent env) (vector-ref env 2))

(define (set-environment-variables! env vars)
  (vector-set! env 0 vars))
(define (set-environment-cells! env cells)
  (vector-set! env 1 cells))

(define the-empty-environment '())


;;; Useful helper function to retrieve the cell associated
;;; with a variable in an environment.
;;; Returns the cell found, #f no variable exists.
(define (get-cell var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
	#f
	(let scan
	    ((vars (environment-variables env))
	     (cells (environment-cells env)))
	  (cond ((null? vars) (plp (environment-parent env)))
		((eq? var (car vars)) (car cells))
		(else (scan (cdr vars) (cdr cells))))))))

;;; Looks for cell of var in env
;;; If not found, finds real scheme value associated with variable
;;; and wraps in default cell
(define (get-variable-cell var env)
  (let ((cell (get-cell var env)))
    (if cell
	cell
	(default-cell (lookup-scheme-value var))))) ;;; JDH TODO FIX

;;; Uses get-variable-cell to find cell, if it exists.
;;; If found, replaces cell contents with given ones
;;; TODO? - create set-cell as its own function?
(define (set-variable-cell! var cell env)
  (let ((current-cell (get-variable-cell var env)))
    (set-cell-value! current-cell (cell-value cell))
    (set-cell-tags! current-cell (cell-tags cell))))

;;; Extension to make underlying Scheme values available to interpreter

(define (lookup-scheme-value var)
  (lexical-reference generic-evaluation-environment var))

(define (define-variable! var cell env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- DEFINE" var) ;should not happen.
      (let scan
	  ((vars (environment-variables env))
	   (cells (environment-cells env)))
	(cond ((null? vars)
	       (set-environment-variables! env (cons var (environment-variables env)))
	       (set-environment-cells! env (cons cell (environment-cells env))))
	      ((eq? var (car vars))
	       (set-car! cells cell))
	      (else
	       (scan (cdr vars) (cdr cells)))))))

