;;; utility.scm - collection of useful utility functions

(declare (usual-integrations))


(define (identity x) x)

(define (any? x) #t)


(define ((compose f g) x) (f (g x)))


;;; This is to keep the Scheme printer from going into an infinite
;;; loop if you try to print a circular data structure, such as an
;;; environment

(set! *unparser-list-depth-limit* 10)
(set! *unparser-list-breadth-limit* 10)

;;; assert, courtesy of GJS
;;; This is part of paranoid programming.
(define (assert p #!optional error-comment irritant)
  (if (not p)
      (begin
	(if (not (default-object? irritant))
	    (pp irritant))
	(error
	 (if (default-object? error-comment)
	     "Failed assertion"
	     error-comment)))))

(define (obj->string obj)
  (with-output-to-string
    (lambda () (write obj))))
