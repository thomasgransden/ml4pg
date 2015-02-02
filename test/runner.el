(defun use-nix-if-present ()
  (dolist (path '("~/.nix-profile/share/emacs/site-lisp"
                  "/run/current-system/sw/share/emacs/site-lisp"))
    (when (and (file-exists-p path)
               (not (member path load-path)))
      (message "Adding %s to load path" path)
      (setq load-path (append load-path (list path))))))

(defun load-proof-general ()
  (unless (fboundp 'coq-mode)
    (message "Loading Proof General")
    (load "ProofGeneral/generic/proof-site")
    (message "Finished loading Proof General")))

(defun ml4pg-reload ()
  (message "Loading ML4PG")
  (load (concat (if (boundp 'home-dir)
                    home-dir
                  (getenv "ML4PG_HOME"))
                "ml4pg.el"))
  (ml4pg-load-coq))

(defun ml4pg-load-tests ()
  (message "Loading ML4PG test suite")
  (let ((load-test     (lambda (f) (load (concat home-dir "test/" f)))))
    (mapcar load-test '("harness.el" "generators.el"))
    (mapcar load-test (directory-files (concat home-dir "test/")
                                       nil
                                       ".*-tests\.el"))))

(defun ml4pg-run-tests ()
  (interactive)
  (message "Running all ML4PG tests")
  (let ((debug-on-error t))
    (funcall (if noninteractive 'ert-run-tests-batch 'ert) "^ml4pg-")))

(defun ml4pg-reload-and-test ()
  (interactive)
  (use-nix-if-present)
  (load-proof-general)
  (ml4pg-reload)
  (ml4pg-load-tests)
  (ml4pg-run-tests))

(message "Running ML4PG test suite")
(ml4pg-reload-and-test)
