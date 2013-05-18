;;; Small file defining the "proper" api for SchLint

(define *g* (create-cfg))
(define rootnode 'rootnode)
(define (rootnode? r) (eq? r rootnode))

(define (print-cfg) (pp-cfg *g*))
(define (draw-cfg) (cfg->dot *g*))
(define (reset-cfg) (set! *g* (create-cfg)))

(define escaped-procs '(exit go pp-cfg print-cfg draw-cfg cfg->dot reset-cfg))
