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
;;;(define (foo)
;;;  (define (bar)
;;;    (baz))
;;;  (bar))
;;;(define (baz) #f)
(define *cfg2* (create-cfg))
(define *foo2 (define-global-func *cfg2* 'foo))
(define *bar2 (define-sub-function *cfg2* *foo2 'bar))
(add-function-call *cfg2* *bar2 'baz)
(add-function-call *cfg2* *foo2 'bar)
(define *baz2 (define-global-func *cfg2* 'baz))

(assert (eq? (cfg:defined-by *baz2) (cfg:get-root *cfg2*)) "baz not in global scope")
(assert (eq? (cfg:defined-by *foo2) (cfg:get-root *cfg2*)) "foo not in global scope")
(assert (eq? (cfg:defined-by *bar2) *foo2) "bar not in foo's scope")

(assert (equal? (cfg:get-defines *foo2) `(,*bar2)) "foo doesn't define bar")
(assert (null? (cfg:get-defines *baz2)) "baz defines something?")
(assert (null? (cfg:get-defines *bar2)) "bar defines something?")

(assert (equal? (cfg:get-peers *foo2) `(,*baz2)) "baz isn't foo's peer")
(assert (equal? (cfg:get-peers *baz2) `(,*foo2)) "foo isn't baz's peer")
(assert (null? (cfg:get-peers *bar2)) "bar has peer")


;;; simple example of adding a call before it's defined
;;;(define (foo)
;;;  (define (bar)
;;;    (baz))
;;;  (bar))
;;;(define (bar)
;;;  (define (baz) #f)
;;;    (baz))
(define *cfg3* (create-cfg))
(define *foo3 (define-global-func *cfg3* 'foo))
(define *bar3 (define-sub-function *cfg3* *foo3 'bar))
(add-function-call *cfg3* *bar3 'baz)
(add-function-call *cfg3* *foo3 'bar)
(define *bar3_2 (define-global-func *cfg3* 'bar))
(define *baz3 (define-sub-function *cfg3* *bar3_2 'baz))
(add-function-call *cfg3* *bar3_2 'baz)


'passed
