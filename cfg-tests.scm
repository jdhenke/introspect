(load "cfg")
(load "utility")

;;; Unit testing
(define *g* (create-cfg))
(assert (graph? *g*))

;;; simple example program
;;;
;;;(define (baz) #f)
;;;(define (foo)
;;;  (define (bar)
;;;    (baz))
;;;  (bar))

(define *cfg* (create-cfg))
(define-global-func *cfg* 'baz)
(define-global-func *cfg* 'foo)
(define-sub-function *cfg* 'foo 'bar)
(add-function-call *cfg* 'foo 'bar)
(add-function-call *cfg* 'bar 'baz)

(pp (cfg:defined-by *cfg* 'bar))
(pp (cfg:defined-by *cfg* 'foo))
(pp (cfg:defined-by *cfg* 'baz))

(pp (cfg:get-defines *cfg* 'foo))
(pp (cfg:get-defines *cfg* 'baz))

(pp (cfg:get-peers *cfg* 'foo))
(pp (cfg:get-peers *cfg* 'baz))
(pp (cfg:get-peers *cfg* 'bar))