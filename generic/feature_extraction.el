(defun export-theorem-comment (result name args)
  (proof-assert-next-command-interactive)
  (export-theorem-aux2 result name args))

(defun export-theorem-deffix (result subcmd args)
  (proof-assert-next-command-interactive)
  (ignore-errors (adddefinition subcmd))
  (export-theorem-aux2 result subcmd args)
  (proof-assert-next-command-interactive))

(defun export-theorem-defined (name result)
  (proof-assert-next-command-interactive)
  (append-to tactic-level     (compute-tactic-result     name))
  (append-to proof-tree-level (compute-proof-tree-result name))
  (when name
    (split-feature-vector name (flat (reverse result))))
  (ignore-errors (addthm name)))
