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

(test-with distinct-numbers-are-distinct
  "No overlap when choosing distinct numbers"
  (list-of (compose 'choose-distinct '1+ (gen-num)))
  (lambda (nums)
    (let ((found nil))
      (dolist (num nums)
        (should-not (member num found))
        (append-to found num)))))

(test-with distinct-numbers-limited
  "Choosing distinct numbers doesn't go beyond the specified limit"
  (lambda ()
    (let ((n (1+ (funcall (gen-num)))))
      (list n (choose-distinct n))))
  (lambda (n nums)
    (dolist (num nums)
      (should (<= num n)))))

(test-with partitions-sum-correctly
  "Partitioning up to a number sums to that number"
  (lambda ()
    (let ((n (1+ (funcall (gen-num)))))
      (list n (choose-partitions n))))
  (lambda (n nums)
    (should (equal n (apply '+ nums)))))

(test-with no-empty-partitions
  "Partitioning up to a number does't include 'empty' partitions"
  (list-of (compose 'choose-partitions '1+ (gen-num)))
  (lambda (nums)
    (dolist (num nums)
      (should (> num 0)))))

(test-with correct-number-of-distincts
  "Ensure we can choose a specific number of distinct choices"
  (lambda ()
    (let* ((n    (1+    (funcall (gen-num))))
           (size (+ 1 n (funcall (gen-num)))))
      (list n (choose-distinct size n))))
  (lambda (n nums)
    (should (equal n (length nums)))))

(test-with correct-number-of-partitions
  "Ensure we can choose a specific number of partitions"
  (lambda ()
    (let* ((n    (1+    (funcall (gen-num))))
           (size (+ 1 n (funcall (gen-num)))))
      (list n (choose-partitions size n))))
  (lambda (n nums)
    (should (equal n (length nums)))))

(test-with proof-to-def
  "Ensure proof-to-def finds definitions"
  nil
  (lambda ()
    (dolist (this-name (coq-example-names))
      (test-msg (format "Looking for %s" this-name))
      (with-coq-example
       `(lambda ()
          (should (equal 1 (point)))
          (proof-to-def ,this-name)
          (let* ((old (point))
                 (new (1+ (search-forward ,this-name)))  ;; Increment for space
                 (str (buffer-substring-no-properties old new)))
            (test-msg (format "MOVED TO '%s'" str))
            (test-msg (format "MATCHING '%s'" (coq-declaration-re-with-name ,this-name)))
            (should (equal 0
                           (string-match (coq-declaration-re-with-name ,this-name)
                                         str)))))))))
