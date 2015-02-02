(defun print-similarities-aux ())

(defun print-similarities-weka-str (n str)
  (print-similarities-weka-aux n (extract-clusters-from-file-aux str)))

(defconst single-line "-----------------------------------------------------------------------------------\n")

(defun print-similarities-weka-aux (n clusters)
  (with-current-buffer "*display*"
    (erase-buffer)
    (insert "Similarities:\n")
    (insert single-line)
    (insert "This lemma is similar to the lemmas:\n")
    (let ((l-s-t (length saved-theorems)))
      (dolist (elem (remove-occurrence (clusters-of-n clusters (nth (1- l-s-t)
                                                                    clusters))
                                       (1+ l-s-t)))
        (if (<= elem l-s-t)
            (progn
              (insert "- ")
              (insert-button-lemma (remove_last_colon (car (nth (1- elem)
                                                                saved-theorems)))))
          (progn
            (setf temp-res (remove_last_colon (format "%s" (print-clusters-weka-namecmd-aux elem))))
            (insert "- ")
            (insert-button-lemma temp-res)))))
    (insert single-line)))

(defun print-clusters-weka-namecmd (elem)
  (setf temp-res (format "%s" (print-clusters-weka-namecmd-aux elem))))

(defun print-clusters-weka-namecmd-aux (elem)
  (shell-command (concat "cat "(expand-file-name "names_temp.txt")
                         " | sed -n '"
                         (format "%s" (- elem (length saved-theorems)))
                         "p'"))

  (with-current-buffer "*Shell Command Output*"
    (beginning-of-buffer)
    (read (current-buffer))
    (read (current-buffer))))

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
    (delete-file nt)
    (with-temp-file nt
      (dolist (elem libs-menus)
        (insert (read-file (concat home-dir "libs/" type "/" elem "_names")))))))

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

(defun show-clusters-of-theorem-aux (func action)
  (interactive)
  (let* ((alg (show-clusters-alg algorithm))
         (gra (case granularity-level
                (2 8)
                (3 15)
                (4 25)
                (5 50)
                (t 5))))
    (setq my-buffer "")
    (setf buf (current-buffer))
    (setf res (extract-info-up-to-here))
    (let* ((tmp1 (show-clusters-of-theorem-data res))
           (tmp2 (if libs-menus
                     (let ((str (add-libraries-notemp)))
                       (add-names)
                       str)
                   ""))
           (tmp  (concat tmp1 tmp2))
           (size (size-notemp tmp)))
      (funcall action)
      (switch-to-display)
      (setf signal 5)
      (let ((arg (funcall func size gra)))
        (print-similarities-weka-str arg (weka-notemp arg tmp)))))
  (send-coq-cmd (format "Unset Printing All")))
