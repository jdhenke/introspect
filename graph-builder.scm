;;; graph-builder.scm - our graph construction procedure

;;; Given a corpus of code, add nodes and edges to create a CFG
;;; for the code

;;; Dependencies
(load "ps4/utils" user-initial-environment)
(load "ps4/ghelper" user-initial-environment)
(load "ps4/syntax" user-initial-environment)
(load "ps4/rtdata" user-initial-environment)
(load "utility")
(load "graph")
(load "cfg")

;;; the main method
;;; usage
;; (build-graph '(define (f x) (* x x)))


(define *g* (create-cfg))
(define rootnode 'rootnode)

(define (build-graph code)
  (bgi code rootnode))

;;; CODE POACHED FROM ANALYZE
;;; A function which, when given an expression returns a combinator
;;; A combinator is a function which given and environment produces
;;; The result of evaluating the expression in the environment
;;; Returns the cell of the answer
(define bgi
  (make-generic-operator 2 'build-graph-inner
    (lambda (code parent-node)
      (cond ((application? code)
	     (begin
	       (pp "bgi-root")
	       (pp code)
	       (pp parent-node)
	       (bgi-application code parent-node)))
	    (else
	     (error "Unknown expression type"
		    code))))))

(define (bgi-self-evaluating code parent-node)
  (begin
    (pp "bgi-self-evaluating")
    ;;(pp code)
    ;;(pp parent-node)
    ))
(defhandler bgi bgi-self-evaluating self-evaluating? any?)

(define (bgi-quoted code parent-node)
  (begin
    (pp "bgi-quoted")
    ;;(pp code)
    ;;(pp parent-node)
    ))
(defhandler bgi bgi-quoted quoted? any?)

(define (bgi-variable code parent-node)
  (begin
    (pp "bgi-variable")
    ;;(pp code)
    ;;(pp parent-node)
    ))
(defhandler bgi bgi-variable variable? any?)

(define (bgi-if code parent-node)
  (begin
    (pp "bgi-if")
    ;;(pp code)
    ;;(pp parent-node)
    ))
(defhandler bgi bgi-if if? any?)

(define (bgi-lambda code parent-node)
  (begin
    (pp "bgi-lambda")
    ;;(pp code)
    ;;(pp parent-node)
    (bgi (lambda-body code) parent-node)))
(defhandler bgi bgi-lambda lambda? any?)

(define (bgi-application code parent-node)
  (begin
    (pp "bgi-application")
    (pp code)
    (pp "Adding function call edge from/to:")
    (pp parent-node)
    (pp (string (operator code)))
    ;; TODO doublecheck (string (operator code)) is
    ;; really the correct name of the function
    ;; TODO create node first? Or check for prior existence?
    (let ((destination-name (string (operator code))))
      (pp (add-function-call *g* parent-node destination-name))
      (bgi (operator code) parent-node)
      (define (bgi-tmp code)
	(bgi code parent-node))
      (map bgi-tmp (operands code)))))

(define (bgi-sequence exps parent-node)
  (begin
    (pp "bgi-sequence")
    ;;(pp exps)
    ;;(pp parent-node)
    (if (null? exps) (error "Empty sequence"))
    (define (bgi-tmp code)
      (bgi code parent-node))
    (map bgi-tmp exps)))

(defhandler bgi
  (lambda (code parent-node)
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
    (let ((this-node
	   (if (eq? parent-node rootnode)
	       (define-global-func *g* (definition-variable code))
	       (define-sub-function *g* parent-node (definition-variable code)))))
      ;; would add "dependency" edge here, distinct
      ;; from function-call edge
      (bgi (definition-value code) this-node))))
(defhandler bgi bgi-definition definition? any?)

;;; Macros (definitions are in syntax.scm)
(defhandler bgi (compose bgi cond->if) cond? any?)

(defhandler bgi (compose bgi let->combination) let? any?)
