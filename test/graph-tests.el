;; Test GraphViz interaction

(test-with clusterofone-empty
  "Empty graphs should produce no code"
  (lambda ())
  (lambda ()
    (should (equal (clusterofone nil 1 nil nil) ""))))

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

(test-with issubcluster
  "Check whether one cluster is an element of another"
  (lambda ()
    "Generate C1, C2 and IS-SUB, such that IS-SUB tells us whether or not all of
     the elements in C1 are contained in C2."
    (let* ((c2            (funcall (gen-list (gen-string))))
           (gen-c2-elem   (if c2 (gen-elem c2) (lambda ())))
           (is-sub        (and c2 (funcall (gen-bool))))
           (gen-not-elem  (gen-filtered (gen-string)
                                       `(lambda (x) (not (member x ',c2)))))
           (gen-c1        (if is-sub gen-c2-elem (gen-string)))
           (pre           (funcall (gen-list gen-c1)))
           (post          (funcall (gen-list gen-c1)))
           (mid           (funcall (if is-sub
                                       (gen-list gen-c2-elem)
                                       (gen-list gen-not-elem
                                                 (compose '1+ (gen-num)))))))
      (list (append pre mid post) c2 is-sub)))
  (lambda (c1 c2 is-sub)
    (should (equal (issubcluster c1 c2) is-sub))))
