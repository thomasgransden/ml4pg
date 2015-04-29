(defun make-goal-string (names goal n)
  "Generates a plausible Coq context. NAMES is a list of (name . type) pairs for
   the hypotheses, GOAL a string for the goal and N is the number of '=' to
   separate them with. In practice N=28, but we encourage robustness. For
   example:

   (make-goal-string '((\"hello\" . \"world\") (\"foo\" . \"bar\"))
                     \"my-goal\"
                     3)

   hello:world
   foo:bar
   ===
   my-goal"
  (concat (join-strings (mapcar (lambda (line)
                                  (concat (car line)   ; Coq name
                                          ":"          ; "has type"
                                          (cdr line))) ; Type
                                names)
                        "\n")
          "\n"
          (make-string n ?=)
          "\n"
          goal))

(test-with get-hypotheses-from
  "Ensure we can recover hypothesis names"
  (list-of (gen-list (gen-pair (gen-coq-name) (gen-string-without ":" "\n")))
           (gen-string)
           (compose '1+ (gen-num)))
  (lambda (hypotheses goal n)
    (let* ((content (make-goal-string hypotheses goal n))
           (result  (get-hypotheses-from content))
           (names   (mapcar 'car hypotheses)))
      (message "NAMES: %S\nRESULT: %S" names result)
      (message "CONTENT\n\n%s\n\nEND CONTENT" content)
      ; All names should appear in the result list
      (dolist (name names)
        (should (member name result)))
      ; All result elements should appear in the name list
      (dolist (elem result)
        (should (member elem names))))))

(defconst example-hypotheses
  '(("app_nil_l"
     (() ("l") ("l") ("l") ("l" "a0" "l0") ()))

    ("app_nil_l_shorter"
     (() ("l") ()))

    ("app_nil_l_shorterb"
     (() ("l") ("l") ()))

    ("app_nil_l2"
     (() ("l") () ("a" "l" "IHl") ("a" "l" "IHl") ("a" "l" "IHl") ()))

    ("app_nil_l2b"
     (() () ("a" "l" "IHl") ("a" "l" "IHl") ("a" "l" "IHl") ()))

    ("mult_n_O"
     (() () ("n" "IHn") ())))
  "Some hypotheses from ml4pg.v")

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
