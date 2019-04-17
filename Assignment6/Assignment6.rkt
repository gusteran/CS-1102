#lang racket
(require test-engine/racket-tests)
;(require ecmascript/types)

;; #:transparent makes structures display a bit nicer
(define-struct graph (name nodes) #:transparent #:mutable)

(define-struct node (name edges) #:transparent #:mutable)

;; *******************************************************
;; you don't need to understand this part
;; just be sure you use my-eval instead of eval
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


(define-syntax my-eval
  (syntax-rules ()
    [(my-eval arg)
     (eval arg ns)]))
;; *******************************************************

;; some examples of Racket weirdness you will need to work around
(define x 3)
(define y 5)
(set! y 6)

;; in the interpreter, try:
;;     (set! x 10)
;;     (set! y 10)
;; first one fails, second one works
;; --> if you want a variable you can change later, will need both
;;     define and set! in your macro
;; (create z 99) could be a handy macro to define and set a variable

(define z (list y))
;          (list 6)
(set! y 8)
;; in the interpreter, try
;;        y
;;        z
;; it looks like values don't update properly.  how can we make it update properly?

(define z2 (list (quote y)))
(set! y 11)
;; what is z2?
;; it's '(y)
;; how to get back the 11?

;; how about (my-eval (first z2)) ?
;;    -> that's how we'll store lists of nodes

;;========================================================================

;; Creates a mutable variable by defining it then applying set!
(define-syntax create
  (syntax-rules ()
    [(create var value)
     (begin (define var 0) (set! var value))]))

;; Graph -> Graph
;; only adds a node to the nodes if its unqiue
(define (add-unique-node graph node)
  (if (false? (member node (graph-nodes graph)))
      (make-graph (graph-name graph) (cons node (graph-nodes graph)))
      graph))

;; Node -> Node
;; only adds an edge to the edges if its unqiue
(define (add-unique-edge node edge)
  (if (false? (member edge (node-edges node)))
      (make-node (node-name node) (cons edge (node-edges node)))
      node))

;; Defines a new graph
(define-syntax new
  (syntax-rules (graph node)
    [(new graph var)
     (create var (make-graph (quote var) empty))]
    [(new node var)
     (create var (make-node (quote var) empty))]))

;; Adds a node to a graph
(define-syntax vertex
  (syntax-rules (in)
    [(vertex n in graph)
     (begin (new node n)
            (set! graph (add-unique-node graph n)))]))

;; Adds an edge to a node
(define-syntax edge
  (syntax-rules ()
    [(edge node edge)
     (set! node (add-unique-edge node (quote edge)))]))

;; Sets edges to node, either in only one direction or in both
;; Can take infinite arguments given correct syntax
(define-syntax edges
  (syntax-rules (-> <->)
    [(edges v1 -> v2)
     (edge v1 v2)]
    [(edges v1 <-> v2)
     (begin (edge v1 v2) (edge v2 v1))]
    [(edges v1 -> v2 e3 ...)
     (begin (edge v1 v2) (edges v2 e3 ...))]
    [(edges v1 <-> v2 e3 ...)
     (begin (edge v1 v2) (edge v2 v1) (edges v2 e3 ...))]))

;; Returns whether a node is an edge of another node
(define-syntax ->?
  (syntax-rules ()
    [(->? node edge)
     (not (false? (member (quote edge) (node-edges node))))]))

;; Returns whether two nodes have each other as an edge
(define-syntax <->?
  (syntax-rules ()
    [(<->? node1 node2)
     (or (->? node1 node2) (->? node2 node1))]))

;; Node Node -> Boolean
;; Returns whether a node can be reached from another node
(define (-->? node edge)
  (local [(define (lon-->? lon edge)
            (cond [(empty? lon) false]
                  [else (or (-->? (my-eval (first lon)) edge)
                            (lon-->? (rest lon) edge))]))]
    (or (not (false? (member (node-name edge) (node-edges  node))))
        (lon-->? (node-edges node) edge))))



;; Graph -> (listof Nodes)
;; returns a list of all of the nodes in a graph
(define (all-nodes graph)
  (local [(define (all-nodes-lon lon acc)
            (cond [(empty? lon) acc]
                  [(false? (member (first lon) acc))
                   (all-nodes-lon (append (rest lon) (node-edges (first lon)))
                                  (cons (node-name (first lon)) acc))]
                  [else (all-nodes-lon (rest lon) acc)]))]
    (all-nodes-lon (graph-nodes graph) empty)))

;; Node Node -> Number
;; returns the distance between two nodes
;; returns -1 if they are not connected
(define (distance node edge)
  (local
    [(define (distance node dist todo done)
       (cond
         [(not (false? (member (node-name edge) (node-edges  node)))) dist]
         [(false? (member node done))
          (distance--lon (add1 dist) (append todo (node-edges node)) (cons (node-name node) done))]
         [else (distance--lon dist todo done)]))
     (define (distance--lon dist todo done)
       (cond [(empty? todo) -1]
             [(false? (member (first todo) done))
              (distance (my-eval (first todo)) dist (rest todo) done)]
             [else (distance--lon dist (rest todo) done)]))]
    (distance node 1 empty empty)))




(new graph g0)
(vertex n0 in g0)
(vertex n1 in g0)
(vertex n2 in g0)

(new graph g1)
(vertex n3 in g1)

(new graph g2)
(vertex n4 in g2)
(vertex n5 in g2)
(vertex n6 in g2)
(vertex n7 in g2)
(vertex n8 in g2)
;(edge n1 n2)
;(edge n0 n1)
;(edges n0 -> n1)
;(edges n0 -> n1 <-> n2)
(edges n0 -> n1 <-> n2 -> n0)
(edges n4 -> n5 -> n6 -> n7 -> n8)

(check-expect (->? n0 n1) true)
(check-expect (-->? n0 n1) true)
(check-expect (->? n0 n2) false)
(check-expect (-->? n0 n2) true)
(check-expect (-->? n4 n8) true)
(check-expect (-->? n6 n5) false)
(check-expect (->? n1 n0) false)
(check-expect (<->? n1 n2) true)
(check-expect (<->? n1 n4) false)

(check-expect (distance n0 n1) 1)
(check-expect (distance n0 n2) 2)
(check-expect (distance n0 n3) -1)
(check-expect (distance n4 n8) 4)
(check-expect (distance n5 n4) -1)
(check-expect (distance n2 n1) 1)
(check-expect (distance n1 n2) 1)

(check-expect (all-nodes g0) '(n0 n1 n2))
(check-expect (all-nodes g1) '(n3))
(test)