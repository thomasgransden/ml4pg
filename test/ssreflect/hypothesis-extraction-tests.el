(test-with get-hypotheses-from
  "Ensure we can recover hypothesis names"
  (list-of (gen-list (gen-pair (gen-coq-name) (gen-string-without ":" "\n")))
           (gen-string)
           (compose '1+ (gen-num)))
  (lambda (hypotheses goal n)
    (let* ((content (make-goal-string hypotheses goal n))
           (result  (get-hypotheses-from content))
           (names   (mapcar 'car hypotheses)))
      (test-msg (format "CONTENT\n%s\nEND CONTENT" content))
      (test-msg (format "RESULT\n%S" result))
      ; All names should appear in the result list
      (dolist (name names)
        (should (member name result)))
      ; All result elements should appear in the name list
      (dolist (elem result)
        (should (member elem names))))))

(test-with get-example-hypotheses
  "Make sure we can get the hypotheses from a known example"
  (list-of (gen-elem example-hypotheses))
  (lambda (definition)
    (ignore-errors (proof-shell-exit t))
    (let ((name       (car  definition))
          (hypotheses (cadr definition)))
      (with-coq-example
       `(lambda ()
          (let ((found nil))
            ;; Jump to this definition (the first occurence of its name)
            ;; Note that this relies on semi-unique names. Defining "foo" after
            ;; "foolish" would break it.
            (proof-shell-start)
            (goto-char (point-min))
            (goto-char (search-forward ,name))
            (proof-goto-point)
            (step-over-proof)

            ;; Extract the hypotheses at each step
            (dolist (hyps ',hypotheses)
              (sit-for 0.5)
              (append-to found (get-hypotheses))
              (proof-assert-next-command-interactive))

            ;; Compare
            (proof-shell-exit t)
            (should (equal found ',hypotheses))))))))

(test-with can-set-hypotheses-file
  "Ensure we can set the file to write hypotheses to"
  (list-of (gen-string))
  (lambda (f)
    (set-hypotheses-file f)
    (let ((hf hypotheses-file))
      (set-hypotheses-file nil)
      (should (equal hf f)))))

(test-with can-read-back-hypotheses
  "Ensure we can read the hypotheses we've written"
  (list-of
   (unsize
    (gen-sized-list
     (gen-sized-list-of (unsized (gen-string))
                        (gen-sized-list (unsized (gen-string)))))))
  (lambda (hyps)
    (let* ((formatted (format-hypotheses hyps))
           (read-back (car (read-from-string formatted))))
      (test-msg (format "HYPS\n\n%S\n\nFORMATTED\n\n%s\n\nREAD BACK\n\n%S"
                        hyps formatted read-back))
      (should (equal hyps read-back)))))

(test-with append-to-hypotheses-name
  "Appending hypotheses for a name will add that name, if necessary"
  (list-of (gen-nonempty-string)
           (unsize (gen-hypotheses))
           (gen-proof-hypotheses))
  (lambda (new-name new-hyps hyps)
    (let* ((result (append-to-hypotheses new-name new-hyps hyps))
           (names  (mapcar 'car result)))
      (should (member new-name names)))))

(test-with empty-names-not-appended
  "Appending an empty name doesn't do anything"
  (list-of (unsize (gen-hypotheses))
           (gen-proof-hypotheses))
  (lambda (hyp hyps)
    (should (equal hyps (append-to-hypotheses "" hyp hyps)))))

(test-with appending-hypotheses-appends-hypotheses
  "If we append hypotheses, they appear at the end"
  (list-of (gen-nonempty-string)
           (unsize (gen-hypotheses))
           (gen-proof-hypotheses))
  (lambda (new-name new-hyps hyps)
    (dolist (def (append-to-hypotheses new-name new-hyps hyps))
      (when (equal new-name (car def))
        (let ((these-hyps (cdr def)))
          (should (equal new-hyps (car (last these-hyps)))))))))

(test-with appended-hypotheses-remain-intact
  "Appending hypotheses never removes anything"
  (list-of (gen-string)
           (unsize (gen-hypotheses))
           (gen-proof-hypotheses))
  (lambda (new-name new-hyps hyps)
    (let* ((result    (append-to-hypotheses new-name new-hyps hyps))
           (new-names (mapcar 'car result)))
      (dolist (def hyps)
        (let ((index (position (car def) new-names)))
          ;; Each old name should be found in new-names
          (should index)
          ;; The hypotheses for each old step should be found in result
          (dotimes (step (length (cdr def)))
            (should (equal (nth step (cdr def))
                           (nth step (cdr (nth index result)))))))))))

(test-with exporting-theorem-appends-hypotheses
  "Exporting the features of a theorem also exports its hypotheses"
  nil
  (lambda ()
    (let ((proof-hypotheses nil)
          (type             nil))
      (dolist (this-name (coq-example-names))
        (test-msg (format "STARTING TO RUN %s" this-name))
        (with-coq-example
         `(lambda ()
            (let ((name ,this-name))
              (test-msg (format "NAME: %S" name))
              (test-msg (format "PRE: %S" proof-hypotheses))
              (proof-to-def name)
              (save-excursion
                (setq type (thing-at-point 'word)))
              (test-msg (format "Now up to: %s" (buffer-substring-no-properties
                                                 (point) (+ 20 (point)))))
              (export-theorem)
              (test-msg (format "POST: %S" proof-hypotheses)))))
        (test-msg (format "STARTING TO TEST %s" this-name))
        (let ((saved (member this-name (mapcar 'car proof-hypotheses))))
          ;; FIXME: I'm sure this is wrong. I can do:
          ;; Fixpoint func (a b c : nat) : d. Proof. foo. Qed.
          (test-msg (format "TYPE %s\nNAME %s\nSAVED %S\nHYPS %S"
                            type this-name saved proof-hypotheses))
          (if (member type '("Definition" "Fixpoint"))
              (should-not saved)
              (should saved)))))))

(test-with example-has-hypotheses
  "Ensure our hypotheses aren't nil after processing ml4pg.v"
  nil
  (lambda ()
    (setq proof-hypotheses nil)
    (setq hypotheses-file nil)
    (with-coq-example
     `(lambda ()
        (should-not proof-hypotheses)
        (goto-char (point-max))
        (ignore-errors (extract-feature-theorems))
        (should proof-hypotheses)))
    (should proof-hypotheses)
    (setq proof-hypotheses nil)))

(test-with extracting-features-writes-hyps-file
  "Ensure our hypotheses get written out after feature extraction"
  (lambda ()
    (list (make-temp-file "ml4pg_hyps" nil "")))
  (lambda (hyp-file)
    (delete-file hyp-file)
    (with-coq-example
     `(lambda ()
        (message "HYPFILE %S" ,hyp-file)
        (setq hypotheses-file ,hyp-file)
        (setq proof-hypotheses nil)
        (goto-char (point-max))
        (ignore-errors (extract-feature-theorems))
        (setq hypotheses-file nil)))
    (should (file-exists-p hyp-file))
    (with-temp-buffer
      (insert-file-contents hyp-file)
      (delete-file hyp-file)
      (should (equal (buffer-string)
                     (format-hypotheses proof-hypotheses))))))
