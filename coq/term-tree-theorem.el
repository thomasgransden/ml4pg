(require 'cl)


;; Obtain definition and clean the term

(defun obtain-theorem (name)
  (send-coq-cmd (format "Check %s" name)))


(defun clean-goal (goal)
  (let* ((clean-term (remove-whitespaces (remove-jumps (subseq goal (+ 28 (search "============================" goal))
                                   (search "(dependent " goal)))))
     (arr (search "->" clean-term :from-end t))
     (comma (search "," clean-term :from-end t))
     (obj (cond ((and arr comma (< arr comma))  (subseq clean-term (+ 1 comma)))
           (arr (subseq clean-term (+ 2 arr)))
            (comma (subseq clean-term (+ 1 comma)))
            (t clean-term))))
    (replace-questionmark (replace-quote obj))))

(defun vars-goal (goal)
  (let* ((clean-vars (remove-jumps (replace-quote (subseq goal (+ 1 (search ")" goal :start2 ( + 1 (search ")" goal)))) (search "============================" goal)) ))))
    (search-vars clean-vars)))

(defun addcurrentgoal ()
  (interactive)
  (send-coq-cmd (format "Unset Printing All."))
  (send-coq-cmd (format "Unset Printing Notations."))
  (let ((iftable (send-coq-cmd (format "Print Table Printing If.")))
    (term nil))
    (if (search "None" iftable)
    nil
      (send-coq-cmd (format "Remove Printing If %s."
                          (subseq iftable (+ 1 (search ":" iftable))))))

    (setf term (send-coq-cmd (format "Focus")))
    (setf listofstatements (append (list (list 'theorem (make-symbol "temp") (thm-to-list (clean-goal term)))) listofstatements))
    (setf listofthmvariables (append (list (list (vars-goal term) )) listofthmvariables)    )
    (if (search "None" iftable)
    nil
      (send-coq-cmd (format "Add Printing If %s."
                          (subseq iftable (+ 1 (search ":" iftable))))))
    (send-coq-cmd (format "Set Printing Notations."))
    (send-coq-cmd (format "Set Printing All."))))
