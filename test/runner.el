(setq debug-on-error t)

;; Load ML4PG if needed. Don't reload, since it unsets edebug instrumentation.
(unless (boundp 'home-dir)
  (load (concat (getenv "ML4PG_HOME") "ml4pg.el")))
(unless (boundp 'saved-theorems)
  (ml4pg-load-coq))

;; Load tests
(mapcar (lambda (f) (load (concat home-dir "test/" f ".el")))
        '("generators" "generator-tests" "helpers" "helper-tests" "unit"))

;; Run tests
(funcall (if noninteractive 'ert-run-tests-batch 'ert) "^ml4pg-")
