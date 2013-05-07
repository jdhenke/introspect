;; need to run (load "load") first, then (load "setup")
(set! the-global-environment
      (extend-environment '() '() the-empty-environment))
;;; Use our own eval and our own environment construct
(set! hook/repl-eval (lambda (input default/env defaul/repl)
		       (eval input the-global-environment)))