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

(test-with subclusters-in
  "Test that subclusters doesn't append a cluster that's already present"
  (compose (lambda (args) (list (cons (cadr args) (car args))
                                (cadr args)))
           (list-of (gen-list (gen-list (gen-string))) (gen-list (gen-string))))
  (lambda (lst elem)
    (should (member elem lst))
    (should (equal lst (subclusters elem lst)))))

(test-with subclusters-not-in
  "Test that subclusters appends a cluster when not already present"
  (gen-filtered (list-of (gen-list (gen-list (gen-string)))
                         (gen-list (gen-string)))
                (lambda (args)
                  (not (or (member    (cadr args)               (car  args))
                           (any-which (car  args) 'issubcluster (cadr args))))))
  (lambda (lst elem)
    (should (not (member elem lst)))
    (should (member elem (subclusters elem lst)))))

(test-with subclusters-sub
  "Test that subclusters replaces a cluster with a subcluster"
  (gen-filtered (compose (lambda (args)
                           (list (cons (append (nth 1 args) (nth 2 args))
                                       (nth 0 args))
                                 (nth 1 args)))
                         (list-of (gen-list (gen-list (gen-string)))
                                  (gen-list (gen-string)
                                            (compose '1+ (gen-num))
                                            )
                                  (gen-list (gen-string)
                                            (compose '1+ (gen-num))
                                            )))
                (lambda (args) (and (not (member (cadr args) (car args)))
                                    (any-which (car  args)
                                               'issubcluster
                                               (cadr args)))))
  (lambda (lst elem)
    (let (
          ;(sc (subclusters elem lst))
          )
                                        ;(should (not (member elem lst)))
                                        ;(should (not (equal lst sc)))
                                        ;(should (equal (length lst) (length sc)))
                                        ;(should (not (member elem sc)))
      )))
