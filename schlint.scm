;;; Small file defining the "proper" api for SchLint

;;; global reference to the cfg being built
(define *g* (create-cfg))
(define rootnode 'rootnode)
(define (rootnode? r) (eq? r rootnode))
;;; verbosity flag
(define *verbose* #f)
;;; global list of tags to be applied before returning from a tail recursion
(define *pending-tags* '())

;;; Some functions should never be recorded in the cfg, it just doesn't make sense.
(define escaped-procs '(exit go pp-cfg print-cfg draw-cfg cfg->dot reset-cfg load))


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

;;; add all accumulated tags from all tail calls till now.
(define (apply-tags return)
  (define (loop)
    (if (not (empty-queue? *pending-tags*))
	(begin (add-cell-tags! return (dequeue *pending-tags*))
	       (loop))))
  (loop))

;;; Public API

;;; pretty print (to the terminal) the current cfg
(define (print-cfg) (pp-cfg *g*))
;;; print the cfg in dot language to terminal
(define (print-dot) (cfg->dot *g*))
;;; write the graph to the specified file (or "graph.dot" if none give) in dot
;;; language. Render graph with "dot -Tpng graph.dot > graph.png;" on linux
(define (draw-cfg #!optional file_path)
  (if (default-object? file_path)
      (cfg->dot *g* "graph.dot")
      (cfg->dot *g* file_path)))
;;; reset the cfg to a blank state. clear is just an alias.
(define (reset-cfg) (set! *g* (create-cfg)))
(define clear-cfg reset-cfg)
