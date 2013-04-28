;;; -*- Mode:Scheme -*- 

(declare (usual-integrations))

;;; We modify the ps4 code to allow arbitrary data to be stored with each variable
;;; let's start with two
;;;  1) it's actualy value
;;;  2) some secret

;;; CURRENT SCHEMA
;;; #(value tags)

(define-structure cell value tags)

(define (default-cell value)
  (make-cell value '()))

(define the-unspecified-value (list 'the-unspecified-value))

(define (true? x)
  (if x true false))

(define (false? x)
  (if x false true))

;;; Primitive procedures are inherited from Scheme.

(define (strict-primitive-procedure? cell)
  (procedure? (cell-value cell)))

(define (apply-primitive-procedure proc-cell args-cells)
  (let* ((proc (cell-value proc-cell)) ;;; MIT scheme object
	 (value (apply proc (map cell-value args-cells))) ;;; mit scheme object
	 (tags (apply append (map cell-tags args-cells)))) ;;; JDH TODO
      (make-cell value tags)))


;;; Compound procedures

(define (make-compound-procedure vars bproc env)
  (vector 'compound-procedure vars bproc env))

(define (compound-procedure? cell)
  (let ((obj (cell-value cell)))
    (vector-compound-procedure? obj)))
  
(define (vector-compound-procedure? obj)
    (and (vector? obj)
	 (eq? (vector-ref obj 0) 'compound-procedure)))


(define (procedure-parameters p) (vector-ref p 1))
(define (procedure-body p) (vector-ref p 2))
(define (procedure-environment p) (vector-ref p 3))

;;; An ENVIRONMENT is a chain of FRAMES, made of vectors.

(define (extend-environment variables cells base-environment)
  (if (fix:= (length variables) (length cells))
      (vector variables cells base-environment) ;this is exactly how a new environmetn is created
      (if (fix:< (length variables) (length cells))
	  (error "Too many arguments supplied" variables cells)
	  (error "Too few arguments supplied" variables cells))))

(define (environment-variables env) (vector-ref env 0))
(define (environment-values env) (vector-ref env 1))
(define (environment-parent env) (vector-ref env 2))

(define the-empty-environment '())

(define (get-variable-cell var env)
  (let ((cell (get-cell var env)))
    (if cell
	cell
	(default-cell (lookup-scheme-value var))))) ;;; JDH TODO FIX

(define (get-cell var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
	#f
	(let scan
	    ((vars (vector-ref env 0))
	     (vals (vector-ref env 1)))
	  (cond ((null? vars) (plp (vector-ref env 2)))
		((eq? var (car vars)) (car vals))
		(else (scan (cdr vars) (cdr vals))))))))

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
	  ((vars (vector-ref env 0))
	   (vals (vector-ref env 1)))
	(cond ((null? vars)
	       (vector-set! env 0 (cons var (vector-ref env 0)))
	       (vector-set! env 1 (cons cell (vector-ref env 1))))
	      ((eq? var (car vars))
	       (set-car! vals cell))
	      (else
	       (scan (cdr vars) (cdr vals)))))))

