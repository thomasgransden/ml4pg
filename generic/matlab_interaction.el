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

(defun add-names-aux (type)
  (let ((nt (expand-file-name "names_temp.txt")))
    (shell-command (concat "rm " nt))
    (shell-command (concat "touch " nt))
    (do ((temp libs-menus (cdr temp)))
        ((endp temp) nil)
      (shell-command  (concat "cat " home-dir "libs/" type "/" (car temp)
                              "_names >> " nt)))))

(defun add-libraries-temp-str (type add)
  (let ((path (concat home-dir "libs/" type "/"))
        (post (cond (((string= level "g") "")
                     ((string= level "t") "_tactics")
                     ((string= level "p") "_tree"))))
        (res  ""))
    (dolist (elem libs-menu res)
      (setq res (concat res (read-file (concat path elem post ".csv"))))
      (when (and add (string= level "g"))
        (add-to-saved-theorems-libs (concat path elem post ".csv"))))))

(defun add-libraries-temp-aux (type add)
  (let ((str (add-libraries-temp-str type add)))
    (with-temp-file (expand-file-name "temp.csv")
      (goto-char (point-max))
      (insert str))))

(defun add-to-saved-theorems-libs (file)
  (add-to-saved-theorems-libs-aux (read-lines file)))

(defun add-to-saved-theorems-libs-aux (lines)
  (setf saved-theorems-libs
        (append saved-theorems-libs
                (mapcar (lambda (x)
                          (mapcar (lambda (y)
                                    (car (read-from-string y)))
                                  (cluster-string-to-list x)))
                        lines))))

(defun size-notemp (str)
  (length (split-string str "\n" t)))

(defun size-temp ()
  (length (read-lines (expand-file-name "temp.csv"))))
