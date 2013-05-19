(define a 2)
(add-tag a 'aa)
(assert (equal? (get-tags a) '(aa)) "basic tag")

(assert (equal? (get-tags (add-tag 1 'temporary)) '(temporary)) "tagging self-evaluating works")
(add-tag 1 'temp)
(assert (not (equal? (get-tags 1) '(temp))) "tagging self-evaluating sticks...")

(add-tag + 'bar)
(assert (equal? (get-tags (+ 1 2)) '(bar)) "tagging primitive procedure")
(assert (equal? (get-tags (+ a a)) '(bar aa)) "checking multiple tags")

;; a tail-recursive list summing procedure
(define (loop lis sum-so-far)
  (cond ((null? lis)
	 sum-so-far)
	(else
	 (loop (cdr lis)
	       (+ sum-so-far (car lis))))))

;; a friendly wrapper that supplies an initial running sum of 0
(define (list-sum lis)
  (loop lis 0))
(assert (equal? (get-tags (list-sum (list a 2))) '(aa bar)))

;; Test that provenance tracking hasn't broken tail recursion...
(define (foo a) (if (> a 20000) a (foo (+ a 1))))
(foo 0)

'passed