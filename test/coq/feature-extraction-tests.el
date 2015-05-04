;; Test feature extraction

(test-with can-extract-theorems
  "Extracting theorems runs"
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (test-msg "RUNNING extract-feature-theorems")
                        (extract-feature-theorems)))))

(test-with can-export-theorem
  "Can export the theorems from ml4pg.v"
  (list-of (gen-num))
  (lambda (n)
    (with-coq-example
     `(lambda ()
        (let* ((names (extract-coq-names-from (buffer-string)))
               (name  (nth (% ,n (length names)) names)))
          (proof-to-def name)
          (test-msg (format "RUNNING export-theorem '%s'" name))
          (export-theorem)
          (test-msg (format "VARIABLES: %S"
                            (list 'tdl1         tdl1
                                  'tdl2         tdl2
                                  'tdl3         tdl3
                                  'tdl4         tdl4
                                  'tdl5         tdl5
                                  'intro        intro
                                  'case         case
                                  'simpltrivial simpltrivial
                                  'induction    induction
                                  'simpl        simpl
                                  'rewrite      rewrite
                                  'trivial      trivial
                                  'hypothesis   hypothesis
                                  'goal-level   goal-level))))))))
