;; Keep a record of the available hypotheses at each proof step, for use by
;; other tools

(defvar proof-hypotheses nil
  "Build up the hypotheses available at each step in each proof")

(defun get-hypotheses ()
  ;; Die if there's no *goals* buffer
  (unless proof-goals-buffer
    (error "No 'goals' buffer to get hypotheses from: %S" (buffer-list)))

  ;; Switch to *goals* buffer
  (with-current-buffer proof-goals-buffer
    (let ((str (buffer-substring-no-properties (point-min) (point-max))))
      (message "GOALS BUFFER:\n%s\n" str)
      (get-hypotheses-from str))))

(defun get-hypotheses-from (str)
  (let ((hypotheses  nil)
        (accumulator nil))
    (dolist (line (split-string str "\n" t) hypotheses)

      ;; When the line contains ":", keep the preceding text
      (let ((colon (search ":" line)))
        (when colon
          (append-to accumulator
                     (remove-whitespace (subseq line 0 colon)))))

      ;; If the line only contains "=", we've run out of hypotheses
      (when (equal "=" (remove-duplicates (remove-whitespace line)))
        (setq hypotheses accumulator)))))
