(load "cfg.scm")
(load "utility.scm")

;;; Unit testing
(define *g* (create-cfg))
(assert (cfg? *g*))

;;; simple example program
;;;
;;;(define (baz) #f)
;;;(define (foo)
;;;  (define (bar)
;;;    (baz))
;;;  (bar))

(define *cfg* (create-cfg))
(define *baz (define-global-func *cfg* 'baz))
(define *foo (define-global-func *cfg* 'foo))
(define *bar (define-sub-function *cfg* *foo 'bar))
(add-function-call *cfg* *bar 'baz)
(add-function-call *cfg* *foo 'bar)

(assert (eq? (cfg:defined-by *baz) (cfg:get-root *cfg*)) "baz not in global scope")
(assert (eq? (cfg:defined-by *foo) (cfg:get-root *cfg*)) "foo not in global scope")
(assert (eq? (cfg:defined-by *bar) *foo) "bar not in foo's scope")

(pp (cfg:get-defines *foo))
(pp (cfg:get-defines *baz))

(pp (cfg:get-peers *foo))
(pp (cfg:get-peers *baz))
(pp (cfg:get-peers *bar))

;;; simple example of adding a call before it's defined
(define *cfg* (create-cfg))
(define *foo (define-global-func *cfg* 'foo))
(define *bar (define-sub-function *cfg* *foo 'bar))
(add-function-call *cfg* *bar 'baz)
(add-function-call *cfg* *foo 'bar)
(define *baz (define-global-func *cfg* 'baz))
