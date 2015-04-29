(defun make-goal-string (names goal)
  (concat (join-strings (mapcar (lambda (line)
                                  (concat (car line)   ; Coq name
                                          ":"          ; "has type"
                                          (cdr line))) ; Type
                                names)
                        "\n")
          "\n======\n"
          goal))

(test-with get-hypotheses-from
  "Ensure we can recover hypothesis names"
  (list-of (gen-list (gen-pair (gen-coq-name) (gen-string)))
           (gen-string))
  (lambda (hypotheses goal)
    (let ((result (get-hypotheses-from (make-goal-string hypotheses goal)))
          (names  (mapcar 'car hypotheses)))
      (message "NAMES: %S\nRESULT: %S" names result)
      ; All names should appear in the result list
      (dolist (name names)
        (should (member name result)))
      ; All result elements should appear in the name list
      (dolist (elem result)
        (should (member elem names))))))
