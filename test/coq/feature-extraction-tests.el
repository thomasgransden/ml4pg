;; Test feature extraction

(test-with can-extract-theorems
  "Extracting theorems runs"
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (test-msg "RUNNING extract-feature-theorems")
                        (extract-feature-theorems)))))

(test-with can-export-theorem
  ""
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (test-msg "RUNNING export-theorem")
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
                                                'goal-level   goal-level)))))))
