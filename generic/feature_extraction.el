(defun export-theorem-comment (result name args)
  (let ((pos (proof-queue-or-locked-end)))
    (proof-assert-next-command-interactive)
    (test-msg (format "ETC %s %s" pos (proof-queue-or-locked-end)))
    (when (equal pos (proof-queue-or-locked-end))
      (test-msg (format "COQ\n%s\nEND COQ" (coq-buffer-contents)))
      (error "Stuck at %s, after %s" pos name)))
  (export-theorem-aux2 result name args))

(defun export-theorem-deffix (result subcmd args)
  (test-msg (format "A %s" (proof-queue-or-locked-end)))
  (proof-assert-next-command-interactive)
  (test-msg (format "B %s" (proof-queue-or-locked-end)))
  (let ((pos (proof-queue-or-locked-end)))
    (adddefinition subcmd)
    (goto-char pos)
    (proof-shell-wait)
    (proof-goto-point)
    (proof-shell-wait)
    (proof-assert-next-command-interactive))
  (test-msg (format "B2 %s" (proof-queue-or-locked-end)))
  ;(export-theorem-aux2 result subcmd args)
  ;(proof-assert-next-command-interactive)
  )

(defun export-theorem-defined (name result)
  (proof-assert-next-command-interactive)
  (append-to tactic-level     (compute-tactic-result     name))
  (append-to proof-tree-level (compute-proof-tree-result name))
  (when name
    (split-feature-vector name (flat (reverse result))))
  (ignore-errors (addthm name)))

(defun get-semis ()
  (test-msg (format "GS POINT %s PROOF %s" (point) (proof-queue-or-locked-end)))
  (goto-char (proof-queue-or-locked-end))
  (save-excursion
    (skip-chars-backward " \t\n"
                         (proof-queue-or-locked-end))
    (proof-segment-up-to-using-cache (point))))

(defun export-theorem-aux2 (result name args)
  (let* ((semis   (get-semis))
         (first   (car semis))
         (comment (nth 0 first))
         (cmd     (nth 1 first))
         (subcmd  (ignore-errors (remove-jumps (between-spaces cmd))))
         (ts      nil))
    (test-msg (format "SEMIS %s" semis))
    (when semis
      (cond ((or (string= comment "comment")
                 (is-in-search cmd)
                 (search "Proof" cmd))
             (export-theorem-comment result name   args))

            ((is-problematic cmd)
             (message "FIXME: Skipping 'problematic' step")
             (export-theorem-problematic))

            ((or (search "Definition" cmd)
                 (search "Fixpoint"   cmd))
             (export-theorem-deffix  result subcmd args))

            ((or (search "Instance"  cmd)
                 (search "Theorem"   cmd)
                 (search "Remark"    cmd)
                 (search "Corollary" cmd)
                 (search "Lemma"     cmd))
             (export-theorem-comment result subcmd args))

            ((or (search "Qed."     cmd)
                 (search "Defined." cmd))
             (export-theorem-defined name result))

            (t
             (export-theorem-otherwise cmd  result name args))))))

(defun get-number-of-goals ()
  (show-pos "Getting goal at")
  (condition-case nil
      (save-proof-point
       (count-seq "?" (do-show-proof)))
    (error 0)))
