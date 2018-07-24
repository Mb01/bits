#lang racket

(require graph)

(define g (weighted-graph/undirected
           '(
             (10 a b)
             (20 b c)
             (100 a d)
             (1 d c)
             )))

(get-neighbors g 'a);

;(edge-weight g 'a 'c) ;gets weight between a and b
;(has-vertex? g 'a)
;(vertex=? g 'a 'b)

(define (spanning-tree g)
  (weighted-graph/undirected
   (map (λ (x)
          (cons (edge-weight g (car x) (cadr x)) x ))
          (mst-kruskal g))))

(set! g (spanning-tree g))

(when #t
  (define outfile (open-output-file "graph" #:exists 'replace))
  (display (graphviz g  #:colors (coloring g 3)))

  (display (graphviz g
                     #:output outfile
                     #:colors (coloring g 3)))
  
  (close-output-port outfile)
  ; you need to install graphviz e.g. run "sudo apt-get install graphviz"
  (system "dot -Tps graph -o graph.ps")
  ; read with postscript reader
  (system "xreader graph.ps"))
