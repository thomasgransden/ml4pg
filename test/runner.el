(defun ml4pg-run-tests ()
  (interactive)
  (let ((debug-on-error t))

    ;; Load ML4PG if needed. Don't reload, since it unsets edebug instrumentation.
    (load (concat (if (boundp 'home-dir)
                      home-dir
                    (getenv "ML4PG_HOME"))
                  "ml4pg.el"))

    (ml4pg-load-coq)

    ;; Load tests
    (mapcar (lambda (f) (load (concat home-dir "test/" f ".el")))
            '("generators" "generator-tests" "harness" "harness-tests" "unit"
              "pure_helper-tests" "impure_helper-tests"))

    ;; Run tests
    (funcall (if noninteractive 'ert-run-tests-batch 'ert) "^ml4pg-")))

(ml4pg-run-tests)
