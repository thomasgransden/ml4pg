(defun ml4pg-reload (type)
  (message "Loading ML4PG")
  (load (concat (if (boundp 'home-dir)
                    home-dir
                    (getenv "ML4PG_HOME"))
                "ml4pg.el"))
  (test-mode type))

(defun test-mode (type)
  (if (equal type "ssreflect")
      (ml4pg-load-ss)
      (ml4pg-load-coq)))

(defun ml4pg-load-tests (type)
  (message "Loading ML4PG test suite")
  (mapcar  (lambda (f) (load (concat home-dir "test/" f)))
          '("harness.el" "generators.el"))
  (let ((load-test `(lambda (f) (load  (concat home-dir "test/" ,type "/" f)))))
    (mapcar load-test (directory-files (concat home-dir "test/" type)
                                       nil
                                       ".*-tests\.el"))))

(defun ml4pg-run-tests ()
  (interactive)
  (message "Running all ML4PG tests")
  (let ((debug-on-error t))
    (funcall (if noninteractive 'ert-run-tests-batch 'ert) "^ml4pg-")))

(defun ml4pg-reload-and-test (type)
  (interactive)
  (ml4pg-reload type)
  (ml4pg-load-tests type)
  (ml4pg-run-tests))

(setenv "ML4PG_TYPE" (or (getenv "ML4PG_TYPE") (getenv "TEST_SUITE") "coq"))
(message "Running ML4PG test suite")
(ml4pg-reload-and-test (getenv "ML4PG_TYPE"))
