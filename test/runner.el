(defun ml4pg-reload ()
  (message "Loading ML4PG")
  (load (concat (if (boundp 'home-dir)
                    home-dir
                  (getenv "ML4PG_HOME"))
                "ml4pg.el"))
  (test-mode))

(defun test-mode ()
  (if (equal test-suite "ssreflect")
      (ml4pg-load-ss)
      (ml4pg-load-coq)))

(defun ml4pg-load-tests ()
  (message "Loading ML4PG test suite")
  (mapcar  (lambda (f) (load (concat home-dir "test/" f)))
          '("harness.el" "generators.el"))
  (let ((load-test `(lambda (f) (load  (concat home-dir "test/" test-suite "/" f)))))
    (mapcar load-test (directory-files (concat home-dir "test/" test-suite)
                                       nil
                                       ".*-tests\.el"))))

(defun ml4pg-run-tests ()
  (interactive)
  (message "Running all ML4PG tests")
  (let ((debug-on-error t))
    (funcall (if noninteractive 'ert-run-tests-batch 'ert) "^ml4pg-")))

(defun ml4pg-reload-and-test ()
  (interactive)
  (ml4pg-reload)
  (ml4pg-load-tests)
  (ml4pg-run-tests))

(message "Running ML4PG test suite")
(defconst test-suite (or (getenv "TEST_SUITE") "coq")
  "Choose which tests to run via ENV")
(ml4pg-reload-and-test)
