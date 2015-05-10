;; Tests for menu items

(test-with top-level-graph-of-defs
  "Generate similarity graph of definitions"
  nil
  (lambda ()
    (let ((names (coq-example-names)))
      (with-coq-example
       `(lambda ()
          (clean-ml4pg-dir)
          (goto-char (point-max))
          (ignore-errors (extract-feature-theorems))
          (dependencygraph-defs)
          (dolist (file '("out.arff" "out_bis.arff" "temp3.arff" "temp.csv"
                          "temp.gv"  "temp.html" "temp.map" "temp.png"))
            (should (file-exists-p file)))
          (let ((temp-gv  (get-file-contents "temp.gv"))
                (temp-map (get-file-contents "temp.map")))
            ;; Should find some clusters (10 is arbitrary)
            (dotimes (n 10)
              (should (search (format "subgraph cluster%s {" (1+ n)) temp-gv)))
            ;; Should find some names (10 is arbitrary)
            (let ((found-gv  nil)
                  (found-map nil))
              (dolist (name ',names)
                (when (string-match (format
                          "^%s .URL=\"\./[^\"]+\".; %s -> .*.style=invis.$"
                          name name)
                        temp-gv)
                  (append-to found-gv name))
                (when (string-match (format
                          "<area %s %s %s title=\"%s\" %s %s>"
                          "shape=\"poly\""
                          "id=\"node[0-9]+\""
                          "href=\"\./[^\"]+\""
                          name
                          "alt=\"\""
                          "coords=\"[0-9,]+\"")
                        temp-map)
                  (append-to found-map name)))
              (should (> (length found-gv)  4))
              (should (> (length found-map) 4))))
          (clean-ml4pg-dir))))))

(test-with top-level-graph-of-lemmas
  "Generate similarity graph of lemma statements"
  nil
  (lambda ()
    (let ((names (coq-example-names)))
      (with-coq-example
       `(lambda ()
          (clean-ml4pg-dir)
          (goto-char (point-max))
          (ignore-errors (extract-feature-theorems))
          (dependencygraph-statements)
          ;; Temp files should be created
          (should (file-exists-p "temp.html"))
          (should (file-exists-p "temp.png"))
          (should (file-exists-p "temp.map"))
          (should (file-exists-p "temp.gv"))
          (let ((html (get-file-contents "temp.html"))
                (map  (get-file-contents "temp.map"))
                (gv   (get-file-contents "temp.gv")))
            ;; We should get some hierarchical clusters (10 is arbitrary)
            (dotimes (n 10)
              (should (search (format "subgraph cluster%s {" (1+ n)) gv)))
            ;; We should get some names and links (10 is arbitrary)
            (let ((found-gv  nil)
                  (found-map nil))
             (dolist (name ',names)
               (when (string-match (format
                         "%s .URL=.\./[a-zA-Z1-9]+\.html#%s..; %s -> .+.style=invis."
                         name name name)
                       gv)
                 (append-to found-gv name))
               (when (string-match (format
                         "<area shape=.poly. id=.node[0-9]+. href=.+ title=.%s. alt=.. coords=.[0-9,]+.>"
                         name)
                       map)
                 (append-to found-map name)))
             (should (> (length found-gv)  10))
             (should (> (length found-map) 10))))
          (clean-ml4pg-dir))))))

(test-with top-level-graph-of-proofs
  "Generate similarity graph of proofs"
  nil
  (lambda ()
    ;; FIXME: Is this feature broken?
    (should nil)))

(test-with top-level-term-tree
  "Generate term tree of a lemma statement"
  (list-of (gen-elem (coq-example-names)))
  (lambda (name)
    (let ((chosen-coq-name name)
          (lib-store-dir   "/tmp/"))
      (with-coq-example
       `(lambda ()
          (clean-ml4pg-dir)
          (goto-char (point-max))
          (ignore-errors (extract-feature-theorems))
          (test-msg "Running showtreegraphthm")
          (showtreegraphthm)
          (test-msg "Finished showtreegraphthm")
          (should (file-exists-p "temp.gv"))
          (let ((result (get-file-contents "temp.gv")))
            (should (string-match "[0-9]+ -> [0-9]+.arrowhead=none.;"
                                  result)))
          (clean-ml4pg-dir))))))

(test-with top-level-show-clusters
  "Show clusters"
  nil
  (lambda ()
    (let ((names (coq-example-names)))
      (with-coq-example
       `(lambda ()
          (clean-ml4pg-dir)
          (goto-char (point-max))
          ;; FIXME: We shouldn't, but that's how the UI behaves!
          (ignore-errors (extract-feature-theorems))
          (show-clusters-bis)
          (let ((result (get-and-kill-display)))
            ;; Header text should appear
            (should (search "We have found the following clusters"
                            result))
            ;; We should always have a few clusters (7 is arbitrary)
            (dotimes (n 7)
              (should (search (format "Cluster %s: (automaton)" (1+ n))
                              result)))
            ;; A few lemmas should appear (10 is arbitrary)
            (let ((found nil))
              (dolist (name ',names)
                (when (string-match (format "Lemma %s (.*patch.*)" name)
                                    result)
                  (append-to found name)))
              (should (> (length found) 10))
              ;; No name should be found twice
              (should (equal (length found)
                             (length (remove-duplicates found))))))
          (clean-ml4pg-dir))))))

(test-with top-level-show-clusters-definitions
  "Show clusters definitions"
  nil
  (lambda ()
    (let ((names (coq-example-names)))
      (with-coq-example
       `(lambda ()
          (clean-ml4pg-dir)
          (goto-char (point-max))
          ;; FIXME: The UI hits an error, but ignores it
          (ignore-errors (extract-feature-theorems))
          (cluster-definitions)
          (let ((result (get-and-kill-display)))
            ;; Header text should appear
            (should (search "We have found the following clusters:"
                            result))
            ;; We should always have a few clusters (7 is arbitrary)
            (dotimes (n 7)
              (should (search (format "Cluster %s" (1+ n))
                              result)))
            ;; A few definitions should appear (4 is arbitrary)
            (let ((found nil))
              (dolist (name ',names)
                (when (string-match (format "Definition %s (library .*)" name)
                                    result)
                  (append-to found name)))
              (should (> (length found) 4))
              ;; NOTE: Some names may appear twice! Probably a bug.
              ))
          (clean-ml4pg-dir))))))

(test-with top-level-show-similar-theorems
  "Show similar theorems"
  nil
  (lambda ()
    ;; FIXME: Is this feature broken?
    (should nil)))

(test-with top-level-show-cluster-libraries
  "Show cluster libraries"
  nil
  (lambda ()
    ;; FIXME: Is this feature broken?
    (should nil)))
