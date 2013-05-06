;;; graph-builder.scm - our graph construction procedure

;;; Given a corpus of code, add nodes and edges to create a CFG
;;; for the code

;;; Dependencies
(load "ui")
(load "graph")
(load "utility")

;;; the main method
;;; example usage:
;;; (build-graph '(define (func) 3))
;;; to use:
;;; (begin
;;;   (cd "/home/dylan/Dropbox/meng/spring2013/6.945/final-introspect/ps4")
;;;   (load "load.scm")
;;;   (cd "/home/dylan/Dropbox/meng/spring2013/6.945/final-introspect")
;;;   (init))
;;; (define (r a b c) 3)

(define *g* (create-cfg))
(define rootnode (cfg:get-root *g*))

(define (build-graph code)
  (bgi code rootnode))

;; (define (build-graph-inner code parent-node)
;;   (let* ((defname (definition-variable code))
;; 	(body (definition-value code))
;; 	((defnode define-sub-function parent-node defname)))
;;     ))


;;; CODE POACHED FROM ANALYZE
;;; A function which, when given an expression returns a combinator
;;; A combinator is a function which given and environment produces
;;; The result of evaluating the expression in the environment
;;; Returns the cell of the answer
(define bgi
  (make-generic-operator 2 'build-graph-inner
    (lambda (code parent-node)
      (cond ((application? exp)
	     (begin
	       (pp "bgi-root")
	       (pp code)
	       (pp parent-node)
	       (bgi-application code parent-node)))
	    (else
	     (error "Unknown expression type"
		    exp))))))

(define (bgi-self-evaluating code parent-node)
  (begin
    (pp "bgi-self-evaluating")
    (pp code)
    (pp parent-node)))
(defhandler bgi bgi-self-evaluating self-evaluating? any?)

(define (bgi-quoted code parent-node)
  (begin
    (pp "bgi-quoted")
    (pp code)
    (pp parent-node)))
(defhandler bgi bgi-quoted quoted? any?)

(define (bgi-variable code parent-node)
  (begin
    (pp "bgi-variable")
    (pp code)
    (pp parent-node)))
(defhandler bgi bgi-variable variable? any?)

(define (bgi-if code parent-node)
  (begin
    (pp "bgi-if")
    (pp code)
    (pp parent-node)))
(defhandler bgi bgi-if if? any?)

(define (bgi-lambda code parent-node)
  (begin
    (pp "bgi-lambda")
    (pp code)
    (pp parent-node)))
(defhandler bgi bgi-lambda lambda? any?)

(define (bgi-application code)
  (begin
    (pp "bgi-application")
    (pp code)
    (pp parent-node)
    (pp "Adding call edge from/to:")
    (pp code)
    (pp (string (operator code)))
    ;;(pp (string? (string (operator code))))
    (bgi (operator code))
    ;; TODO fix this map to handle passing parent-node
    (map bgi (operands code))))

(define (bgi-sequence exps parent-node)
  (pp "bgi-sequence")
  (pp exps)
  (pp parent-node)
  (if (null? exps) (error "Empty sequence"))
  ;; TODO fix this map to handle passing parent-node
  (map bgi exps))

(defhandler bgi
  (lambda (exp)
    (bgi-sequence (begin-actions exp)))
  begin? any?)

(define (bgi-assignment code parent-node)
  (begin
    (pp "bgi-assignment")
    (pp code)
    (pp parent-node)
    ;; (let ((var (assignment-variable code))
    ;; 	  (vproc (bgi (assignment-value code))))
    (bgi (assignment-value code))))
(defhandler bgi bgi-assignment assignment? any?)

(define (bgi-definition code parent-node)
  (begin
    (pp "bgi-definition")
    (pp code)
    (pp parent-node)
    (pp "Creating node")
    (pp (definition-variable code))
    (pp (definition-value code))
    (bgi (definition-value code))))
(defhandler bgi bgi-definition definition? any?)

;;; Macros (definitions are in syntax.scm)
(defhandler bgi (compose bgi cond->if) cond? any?)

(defhandler bgi (compose bgi let->combination) let? any?)
