(defun ml4pg-run-tests ()
  (interactive)
  (let ((debug-on-error t)
        (load-test     (lambda (f) (load (concat home-dir "test/" f)))))

    ;; Load ML4PG
    (load (concat (if (boundp 'home-dir)
                      home-dir
                    (getenv "ML4PG_HOME"))
                  "ml4pg.el"))

    ;; Load Coq-specific code
    (ml4pg-load-coq)

    ;; Load test harness
    (mapcar load-test '("harness.el" "generators.el"))

    ;; Load tests (files ending in -tests.el)
    (mapcar load-test (directory-files (concat home-dir "test/")
                                       nil
                                       ".*-tests\.el"))

    ;; Run ML4PG tests
    (funcall (if noninteractive 'ert-run-tests-batch 'ert) "^ml4pg-")))

(ml4pg-run-tests)
