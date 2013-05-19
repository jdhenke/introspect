;;; Small file defining the "proper" api for SchLint

(define *g* (create-cfg))
(define *pending-tags* '())
(define rootnode 'rootnode)
(define (rootnode? r) (eq? r rootnode))
(define *verbose* #f)

(define (print-cfg) (pp-cfg *g*))
(define (draw-cfg) (cfg->dot *g*))
(define (reset-cfg) (set! *g* (create-cfg)))

(define escaped-procs '(exit go pp-cfg print-cfg draw-cfg cfg->dot reset-cfg))


;;; Ensure tag is added iff. is not already present
(define (add-cell-tag! cell tag)
  (let loop ((tags (cell-tags cell)))
    (if (null? tags)
	(set-cell-tags! cell (cons tag (cell-tags cell)))
	(if (not (eq? tag (car tags)))
	    (loop (cdr tags)))))
  cell)

(define (add-cell-tags! cell tags)
  (for-each (lambda (tag) (add-cell-tag! cell tag)) tags)
  cell)

(define (apply-tags return)
  (define (loop)
    (if (not (empty-queue? *pending-tags*))
	(begin (add-cell-tags! return (dequeue *pending-tags*))
	       (loop))))
  (loop))