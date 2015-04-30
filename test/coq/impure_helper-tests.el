(test-with append-to
  "Check we can append to variables"
  (list-of (gen-string) (gen-list (gen-string)))
  (lambda (s l)
    (append-to l s)
    (should (member s l))))

(test-with process-with-cmd
   "Test external commands with stdin/out handling"
   nil
   (lambda ()
     (should (equal "b\nd\nf\ni\n"
                    (process-with-cmd "grep"
                                      "abcd\nefghijk"
                                      nil
                                      "-o"
                                      "[bdfi]")))))

(test-with name-from-buf
  "Test getting a filename from a buffer"
  (lambda ()
    (let* ((prefix (funcall (gen-string-without "."))))
      (list prefix
            (concat prefix "." (funcall (gen-string-without "."))))))
  (lambda (prefix full)
    (with-temp-buffer
      (let ((name (rename-buffer full t)))
        (should (equal (name-from-buf)
                       (if (search "." name)
                           (car (string-split name "."))
                           name)))))))

(test-with step-over-proof
  "We go forwards by one commands iff it's 'Proof.'"
  (list-of (gen-num))
  (lambda (n)
    (ignore-errors (proof-shell-exit t))
    (with-coq-example
     `(lambda ()
        ;; Choose the nth occurence of "Proof."
        (proof-shell-start)
        (goto-char (point-min))
        (search-forward "Proof.")
        (let* ((matches (match-data t))
               (index   (% ,n (1- (length matches))))  ;; skip last element
               (match   (nth index matches)))

          ;; Ensure we step over the "Proof."
          (goto-char (- match (length "Proof.")))
          (proof-goto-point)

          (let ((start (proof-queue-or-locked-end)))
            (step-over-proof)
            (should (> (proof-queue-or-locked-end) start)))

          ;; Move away from the "Proof." command and ensure we *don't* step over
          (proof-assert-next-command-interactive)
          (proof-assert-next-command-interactive)
          (let ((start (proof-queue-or-locked-end)))
            (step-over-proof)
            (should (equal (proof-queue-or-locked-end) start))))))))
