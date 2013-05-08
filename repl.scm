;;;; Read-eval-print loop for extended Scheme interpreter

(declare (usual-integrations write write-line pp eval))

(define write
  (make-generic-operator 1 'write
    (access write user-initial-environment)))

(define write-line
  (make-generic-operator 1 'write-line
    (access write-line user-initial-environment)))

(define pp
  (make-generic-operator 1 'pretty-print
    (access pp user-initial-environment)))

(define (procedure-printable-representation procedure)
  `(compound-procedure
    ,(procedure-parameters procedure)
    ,(procedure-body procedure)
    <procedure-environment>))

(defhandler write
  (compose write procedure-printable-representation)
  vector-compound-procedure?)

(defhandler write-line
  (compose write-line procedure-printable-representation)
  vector-compound-procedure?)

(defhandler pp
  (compose pp procedure-printable-representation)
  vector-compound-procedure?)


(define (read) (prompt-for-command-expression "eval> "))

(define the-global-environment 'not-initialized)

(define (init)
  (set! the-global-environment
	(extend-environment '() '() the-empty-environment))
  (define default-repl-eval hook/repl-eval)

;;; Use our own eval and our own environment construct
  (set! hook/repl-eval (lambda (input default/env default/repl)
			 (cell-value (eval input the-global-environment))))
)
