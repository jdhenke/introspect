;;; utility.scm - collection of useful utility functions

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
