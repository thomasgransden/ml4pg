;; Test feature extraction

(test-with can-extract-theorems
  "Extracting theorems runs"
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (test-msg "RUNNING extract-feature-theorems")
                        (extract-feature-theorems)))))

(test-with can-export-theorem
  "Can export a theorem from ml4pg.v"
  (list-of (gen-num))
  (lambda (n)
    (let ((name (nth (% n (length (coq-example-names)))
                     (coq-example-names))))
      (with-coq-example
       `(lambda ()
          (proof-to-def ,name)
          (export-theorem))))))

(test-with can-export-all-theorems
  "Can export the theorems from ml4pg.v"
  nil
  (lambda ()
    (dolist (this-name (coq-example-names))
      (test-msg (format "CAN EXPORT %s?" this-name))
      (with-coq-example
       `(lambda ()
          (let ((name ,this-name))
            (proof-to-def name)
            (message "RUNNING export-theorem '%s'" name)
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
                                    'goal-level   goal-level)))))))))

(test-with can-extract-info-up-to-point
  "Ensure extract-info-up-to-point advances the proof marker"
  (list-of (gen-num))
  (lambda (n)
    (with-coq-example
     `(lambda ()
        (should (equal 1 (proof-queue-or-locked-end)))
        (goto-char (+ 50 (% ,n (point-max))))
        (extract-info-up-to-here)
        (should (> (proof-queue-or-locked-end) 1))))))

(test-with can-extract-feature-theorems
  "Ensure extract-feature-theorems advances the proof marker"
  (list-of (gen-num))
  (lambda (n)
    (with-coq-example
     `(lambda ()
        (should (equal 1 (proof-queue-or-locked-end)))
        (goto-char (+ 50 (% ,n (point-max))))
        (extract-feature-theorems)
        (should (> (proof-queue-or-locked-end) 1))))))
