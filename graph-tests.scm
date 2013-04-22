(load "graph.scm")
(load "utility.scm")

;;; Basic node/edge creation/deletion test for graph API
;;; Run using `(load "graph-tests.scm")`

(define g (create-graph))
(define n1 (add-node g))
(define n2 (add-node g))

(assert (exists-node? g n1) "adding node 1")

(assert (exists-node? g n2) "adding node 2")
(assert (not (eq? n1 n2)) "nodes are distinct")

(assert (not (exists-edge? n1 n2)) "adding edge 1")
(assert (not (exists-edge? n2 n1)) "adding edge 2")

(define e1 (add-edge n1 n2))
(assert (exists-edge? n1 n2) "adding edge 1 in correct direction")
(assert (not (exists-edge? n2 n1)) "adding edge 1 in correct direction (2)")

(add-edge n2 n1)
(assert (exists-edge? n1 n2) "adding edge 2 maintains old edge")
(assert (exists-edge? n2 n1) "adding edge 2 creates new edge" )

(remove-edge! e1)
(assert (not (exists-edge? n1 n2)) "remove edge 1")
(assert (exists-edge? n2 n1) "removing edge 1 keeps edge 2")


(remove-node! g n1)
(assert (= 1 (length (graph-nodes g))) "removes at least *a* node")
(assert (eq? n2 (car (graph-nodes g))) "node 2 remains")
(assert (not (eq? n1 (car (graph-nodes g)))) "node 1 does not remain")
(assert (exists-node? g n2) "removing node 1 leaves node 2")
(assert (not (exists-node? g n1)) "removing node 1 works")
(assert (not (exists-edge? n1 n2)) "removing node 1 keeps node 2")
(assert (not (exists-edge? n2 n1)) "removing node 1 removes edge 2")

'passed