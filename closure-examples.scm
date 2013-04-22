;;; Here's a contrived example and a somewhat useful example of closures

;;; CONTRIVED EXAMPLE

;;; Original function - will be redefined soon
(define (foo) 'original)
(foo)

;;; Define something else using a closure
;;; Note, this lambda is called
(define foo-closure ((lambda () foo)))
(foo-closure)

;;; Redefine function
(define (foo) 'new)

;;; Redefined function reflects new definition
(foo)

;;; The one created via a closure has not
(foo-closure)

;;; USEFUL EXAMPLE

;;; Create a thunk that counts up each time it's called
;;; *without* global variables.
(define gen-next-num
  (let ((x 0))
    (lambda ()
      (set! x (+ x 1))
      x)))

(gen-next-num)
(gen-next-num)
(gen-next-num)

;;; Essentially, the function now has state which is 
;;; literally impossible to mutate from the outside.
;;; Comes up a lot in javascript i think.