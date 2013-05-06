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
(define bar->baz (add-function-call *cfg* *bar 'baz))
(define foo->bar (add-function-call *cfg* *foo 'bar))
(add-execution foo->bar '() '())
(add-execution bar->baz '() '())
(add-execution bar->baz '() '())


(assert (eq? (cfg:defined-by *baz) (cfg:get-root *cfg*)) "baz not in global scope")
(assert (eq? (cfg:defined-by *foo) (cfg:get-root *cfg*)) "foo not in global scope")
(assert (eq? (cfg:defined-by *bar) *foo) "bar not in foo's scope")

(assert (equal? (cfg:get-defines *foo) `(,*bar)) "foo doesn't define bar")
(assert (null? (cfg:get-defines *baz)) "baz defines something?")
(assert (null? (cfg:get-defines *bar)) "bar defines something?")

(assert (equal? (cfg:get-peers *foo) `(,*baz)) "baz isn't foo's peer")
(assert (equal? (cfg:get-peers *baz) `(,*foo)) "foo isn't baz's peer")
(assert (null? (cfg:get-peers *bar)) "bar has peer")

(assert (= (length (get-executions foo->bar)) 1))
(assert (= (length (get-executions bar->baz)) 2))


;;; simple example of adding a call before it's defined
(define *cfg* (create-cfg))
(define *foo (define-global-func *cfg* 'foo))
(define *bar (define-sub-function *cfg* *foo 'bar))
(add-function-call *cfg* *bar 'baz)
(add-function-call *cfg* *foo 'bar)
(define *baz (define-global-func *cfg* 'baz))

(assert (eq? (cfg:defined-by *baz) (cfg:get-root *cfg*)) "baz not in global scope")
(assert (eq? (cfg:defined-by *foo) (cfg:get-root *cfg*)) "foo not in global scope")
(assert (eq? (cfg:defined-by *bar) *foo) "bar not in foo's scope")

(assert (equal? (cfg:get-defines *foo) `(,*bar)) "foo doesn't define bar")
(assert (null? (cfg:get-defines *baz)) "baz defines something?")
(assert (null? (cfg:get-defines *bar)) "bar defines something?")

(assert (equal? (cfg:get-peers *foo) `(,*baz)) "baz isn't foo's peer")
(assert (equal? (cfg:get-peers *baz) `(,*foo)) "foo isn't baz's peer")
(assert (null? (cfg:get-peers *bar)) "bar has peer")

'passed