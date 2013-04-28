;;; -*- Mode:Scheme -*- 

(declare (usual-integrations))

(define the-unspecified-value (list 'the-unspecified-value))

(define (true? x)
  (if x true false))

(define (false? x)
  (if x false true))

;;; Primitive procedures are inherited from Scheme.

(define strict-primitive-procedure? procedure?)
(define apply-primitive-procedure apply)


;;; Compound procedures

(define (make-compound-procedure vars bproc env)
  (vector 'compound-procedure vars bproc env))

(define (compound-procedure? obj)
  (and (vector? obj)
       (eq? (vector-ref obj 0) 'compound-procedure)))

(define (procedure-parameters p) (vector-ref p 1))
(define (procedure-body p) (vector-ref p 2))
(define (procedure-environment p) (vector-ref p 3))

;;; An ENVIRONMENT is a chain of FRAMES, made of vectors.

(define (extend-environment variables values base-environment)
  (if (fix:= (length variables) (length values))
      (vector variables values base-environment) ;this is exactly how a new environmetn is created
      (if (fix:< (length variables) (length values))
	  (error "Too many arguments supplied" variables values)
	  (error "Too few arguments supplied" variables values))))

(define (environment-variables env) (vector-ref env 0))
(define (environment-values env) (vector-ref env 1))
(define (environment-parent env) (vector-ref env 2))

(define the-empty-environment '())

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

(define (lookup-variable-value var env)
  (let ((cell (get-cell var env)))
    (if cell
	(cell-value cell)
	(lookup-scheme-value var))))

;;; Extension to make underlying Scheme values available to interpreter

(define (lookup-scheme-value var)
  (lexical-reference generic-evaluation-environment var))

;;; We modify the ps4 code to allow arbitrary data to be stored with each variable
;;; let's start with two
;;;  1) it's actualy value
;;;  2) some secret

;;; CURRENT SCHEMA
;;; #(value tags)

(define-structure cell value tags)

(define (define-variable! var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- DEFINE" var) ;should not happen.
      (let scan
	  ((vars (vector-ref env 0))
	   (vals (vector-ref env 1)))
	(cond ((null? vars)
	       (vector-set! env 0 (cons var (vector-ref env 0)))
	       (vector-set! env 1 (cons (make-cell val '()) (vector-ref env 1))))
	      ((eq? var (car vars))
	       (set-car! vals (make-cell val '()))) ;;; rethink this decision
	      (else
	       (scan (cdr vars) (cdr vals)))))))


;;; So environments are stored as nested vectors
;;; at each level, should look something like
;;;   #(vars vals next-environment-up)
(define (set-variable-value! var val env)
  (let ((cell (get-cell var env)))
    (if cell
	(set-cell-value! val)
	(error "Unbound variable -- SET!" var))))


;;; Now the fun stuff
(define (add-variable-tag var tag env)
  (let ((cell (get-cell var env)))
    (if cell
	(let loop ((tags (cell-tags cell)))
	  (if (null? tags)
	      (begin
		(set-cell-tags! cell (cons tag (cell-tags cell)))
		#t)
	      (if (eq? tag (car tags))
		  #f
		  (loop (cdr tags)))))
	(error "No variable cell found when setting tags!" var))))

(define (get-variable-tags var env)
  (let ((cell (get-cell var env)))
    (if cell
	(cell-tags cell)
	(error "No variable cell found when getting tags!" var))))