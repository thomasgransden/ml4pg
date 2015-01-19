;; Test GraphViz interaction

(test-with clusterofone-empty
  "Empty graphs should produce no code"
  nil
  (lambda ()
    (should (equal (clusterofone nil 1 nil nil) ""))))

(test-with clusterofone-singleton
  "Graph of one node should have no edges"
  nil
  (lambda ()
    ;(clusterofone lst 1 tbl)
    ))

(test-with clusterofone-node
  "Check dot code for clusters"
  nil
  (lambda ()
    ;(clusterofone-node elem tbl 1)
    ))

(test-with clusterofone-leaf
  "Check dot code for leaves"
  nil
  (lambda ()))

(test-with issubcluster
  "Check whether one cluster is an element of another"
  (lambda ()
    "Generate C1, C2 and IS-SUB, such that IS-SUB tells us whether or not all of
     the elements in C1 are contained in C2."
    (let* ((c2     (funcall (gen-list (gen-string))))
           (is-sub (and c2  (funcall  (gen-bool))))
           (c1     (funcall (gen-list (if is-sub (gen-elem c2) (gen-string)))))
           (elem   (funcall (gen-filtered (gen-string)
                                         `(lambda (x) (not (member x ',c2))))))
           (index  (if c1 (random (length c1)) 0)))
      (list (if is-sub
                c1
                (if c1 (replace-nth c1 index elem)
                       (list elem)))
            c2
            is-sub)))
  (lambda (c1 c2 is-sub)
    (should (equal (issubcluster c1 c2) is-sub))))
