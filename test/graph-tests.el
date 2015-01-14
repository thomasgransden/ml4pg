;; Test GraphViz interaction

(test-with clusterofone-empty
  "Empty graphs should produce no code"
  (lambda ())
  (lambda ()
    (should (equal (clusterofone nil 1 nil) ""))))

(test-with clusterofone-singleton
  "Graph of one node should have no edges"
  (lambda ())
  (lambda ()
    ;(clusterofone lst 1 tbl)
    ))

(test-with clusterofone-node
  "Check dot code for clusters"
  (lambda ())
  (lambda ()
    ;(clusterofone-node elem tbl 1)
    ))

(test-with clusterofone-leaf
  "Check dot code for leaves"
  (lambda ())
  (lambda ()))
