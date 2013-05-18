;;; Small file defining the "proper" api for SchLint

(define *g* (create-cfg))
(define rootnode 'rootnode)
(define (rootnode? r) (eq? r rootnode))

(define (print-cfg) (pp-cfg *g*))
(define (draw-cfg) (cfg->dot *g*))
(define (reset-cfg) (set! *g* (create-cfg)))

(define escaped-procs '(exit go pp-cfg print-cfg draw-cfg cfg->dot reset-cfg))


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
	(let ((new-cell (default-cell (lookup-scheme-value var))))
	  (let loop ((env env))
	    (if (eq? (environment-parent env) the-empty-environment)
		 (define-variable! var new-cell env)
		 (loop (environment-parent env))))
	  new-cell))))

;;; Uses get-variable-cell to find cell, if it exists.
;;; If found, replaces cell contents with given ones
(define (set-variable-cell! var cell env)
  (let ((current-cell (get-variable-cell var env)))
    (set-cell-value! current-cell (cell-value cell))
    (set-cell-tags! current-cell (cell-tags cell))))

;;; Ensure tag is added iff. is not already present
(define (add-cell-tag! cell tag)
  (let loop ((tags (cell-tags cell)))
    (if (null? tags)
	(set-cell-tags! cell (cons tag (cell-tags cell)))
	(if (not (eq? tag (car tags)))
	    (loop (cdr tags)))))
  cell)

(define (add-cell-tags! cell tags)
  (pp tags)
  (for-each (lambda (tag) (add-cell-tag! cell tag)) tags)
  cell)
