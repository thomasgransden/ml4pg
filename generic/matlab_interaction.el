(defun print-similarities-aux ())

(defun print-similarities-weka-str (n str)
  (print-similarities-weka-aux n (extract-clusters-from-file-aux str)))

(defun print-similarities-weka-aux (n clusters)
  (with-current-buffer "*display*"
    (erase-buffer)
    (insert "Similarities:\n")
    (insert "-----------------------------------------------------------------------------------\n")
    (insert "This lemma is similar to the lemmas:\n")
    (do ((temp2 (remove-occurrence (clusters-of-n clusters (nth (1- (length saved-theorems))
                                                                clusters))
                                   (1+ (length saved-theorems)))
                (cdr temp2)))
        ((endp temp2))
      (if (<= (car temp2) (length saved-theorems))
          (progn (insert "- ")
                 (insert-button-lemma (remove_last_colon (car (nth (- (car temp2)
                                                                      1)
                                                                   saved-theorems)))))
        (progn (shell-command (concat "cat "(expand-file-name "names_temp.txt")
                                      " | sed -n '"
                                      (format "%s" (- (car temp2)
                                                      (length saved-theorems)))
                                      "p'"))
               (with-current-buffer "*Shell Command Output*"
                 (beginning-of-buffer)
                 (read (current-buffer))
                 (setf temp-res (remove_last_colon (format "%s"  (read (current-buffer))))))
               (insert "- ")
               (insert-button-lemma temp-res))))
    (insert "-----------------------------------------------------------------------------------\n")))

(defun print-clusters-weka-namecmd (elem)
  (shell-command (concat "cat "(expand-file-name "names_temp.txt")
                         " | sed -n '"
                         (format "%s" (- elem (length saved-theorems)))
                         "p'"))

  (with-current-buffer "*Shell Command Output*"
    (beginning-of-buffer)
    (read (current-buffer))
    (setf temp-res (format "%s" (read (current-buffer))))))

(defun insert-button-automaton-macro2 (l l2)
  (list 'lambda '(x)
        (list 'generate-automaton2 l l2)))

(defun insert-button-automaton2 (l l2)
  (progn (insert-button "automaton" 'action (insert-button-automaton-macro2 (list 'quote l)
                                                                            (list 'quote l2))
                        'face (list 'link)
                        'follow-link t)))
