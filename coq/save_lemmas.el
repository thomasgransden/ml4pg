(defun proof-assert-next-command-interactive3 ()
  (interactive)
  (if (get-buffer "*response*")
  (if (eq save-automatically 0)
      (proof-assert-next-command-interactive)
    (progn (with-current-buffer "*response*"
            (beginning-of-buffer)
                    (if (zerop (buffer-size))
                      (setf temp nil)
              (setf temp (search "No"
                       (format "%s" (read (current-buffer)))))))
          (if temp
            (export-previous-lemm)
                    (proof-assert-next-command-interactive)
              ))

  )
  (proof-assert-next-command-interactive)))


(defun export-previous-lemm ()
  (interactive)
  (let ((final (point))
    (result nil)
    (end nil))
    (search-backward "Proof.")
    (proof-goto-point)
    (while (< (point) final)
      (let* ((semis (save-excursion
              (skip-chars-backward " \t\n"
                       (proof-queue-or-locked-end))
              (proof-segment-up-to-using-cache (point))))
         (comment (caar semis))
         (cmd (cadar semis))
         (ts nil))
    (progn (setf ts (get-top-symbol))
           (setf ng (get-number-of-goals))
           (proof-assert-next-command-interactive)
           (setf ng2 (get-number-of-goals))
           (if cmd
           (setf result (cons (append (get-numbers cmd) (list ts) (list ng2)) result))
           )
            )

    )
    )
    (proof-assert-next-command-interactive)
    (setf saved-theorems (append saved-theorems
                 (list (list (format "%s" (get-name))
                         (flat (reverse result))))))
    (search-forward "Qed.")

  ))


(defun get-name ()
  (search-backward "Lemma")
  (read (current-buffer))
  (read (current-buffer)))


(defun list-to-string (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) temp2)
      (setf temp2 (concat temp2 (car temp) ", "))))

;; FIXME: Name conflict
(defun save-numbers ()
  (ignore-errors
    (save-numbers-aux "coq"
                      'extract-names2
                      (lambda ()
                        (setf buf (buffer-name))
                        (setf name (if (search "." buf) (subseq buf 0 (search "." buf)) buf))
                        (ignore-errors (extract-feature-theorems))))))
