;;; Weka invokation

(defvar whysimilar nil)

(defun convert-all-thms-to-weka-format-several ()
  (setf thms-vectors nil)
  (add-several-libraries-thms)
  (transform-thms)
  (convert-recursive-several-libraries-thms)
  (do ((temp (last-part-of-lists thms-vectors) (cdr temp))
     (temp2 ""))
    ((endp temp) temp2)
    (setf temp2 (concat temp2 (format "%s\n"  (print-list  (car temp) ))))))


(defun weka-thms ()
  (let ((alg (cond ((string= "k" algorithm) "SimpleKMeans")
           ((string= "e" algorithm) "EM")
           ((string= "f" algorithm) "FarthestFirst")))
    (n 0))
    (shell-command (concat "rm " (expand-file-name "temp.csv")))
    (with-temp-file (expand-file-name "temp.csv") (insert (convert-all-thms-to-weka-format-several)))
    (setf n (cond  ((eq 2 granularity-level) (floor (length tables-thms) 7))
          ((eq 3 granularity-level) (floor (length tables-thms) 5))
          ((eq 4 granularity-level) (floor (length tables-thms) 4))
          ((eq 5 granularity-level) (floor (length tables-thms) 2))
          (t (floor (length tables-thms) 8))))

    (shell-command  (concat "sleep 1; cat " home-dir "aux_files/headersdefs.txt "
                (expand-file-name "temp.csv") " > "
                (expand-file-name "temp3.arff")))
    (shell-command (concat "sleep 1; java -classpath "
               *weka-dir*
               " weka.filters.unsupervised.attribute.AddCluster -W \"weka.clusterers." alg " -N " (format "%s" n) " -S 42\" -I last -i "
             (expand-file-name "temp3.arff") " -o " (expand-file-name "out.arff")))
    (shell-command (concat "tail -n +56 "
               (expand-file-name "out.arff") " > " (expand-file-name "out_bis.arff")   ))

    (if whysimilar
    (shell-command (concat "java -classpath "
             *weka-dir*
             " weka.attributeSelection.InfoGainAttributeEval -s \"weka.attributeSelection.Ranker -T 0 -N 5\" -i "
             (expand-file-name "out.arff") " > " (expand-file-name "whysimilar.txt")))
    )
    ))

;;; Printing clusters


(defun print-clusters-weka-defs (gra)
  (let* ((clusters (extract-clusters-from-file-defs ))
     (res1 (remove-nil (remove-alone (cdr (form-clusters clusters gra))))))
    (with-current-buffer "*display*"
      (erase-buffer)
      (insert (format "We have found the following clusters:\n" ))
      (insert (format "------------------------------------------------------------------------------------------------\n" ))

      (do ((temp res1 (cdr temp))
       (i 1 (1+ i)))
      ((endp temp) (insert (format "------------------------------------------------------------------------------------------------\n")) )
      (progn (insert (format "Cluster %s\n" i ))
         (do ((temp2 (car temp) (cdr temp2)))
             ((endp temp2) (insert (format "\n")))
             (progn (insert (format "Definition %s (library %s)\n" (car (nth (1- (car temp2)) tables-definitions))
                        (library-belong (1- (car temp2)))))

                )
             )
         (insert (format "\n"))))
      )))



(defun print-clusters-weka-thms (gra)
  (let* ((clusters (extract-clusters-from-file-defs ))
     (res1 (remove-nil (remove-alone (cdr (form-clusters clusters gra))))))
    (with-current-buffer "*display*"
      (erase-buffer)
      (insert (format "We have found the following clusters:\n" ))
      (insert (format "------------------------------------------------------------------------------------------------\n" ))

      (do ((temp res1 (cdr temp))
       (i 1 (1+ i)))
      ((endp temp) (insert (format "------------------------------------------------------------------------------------------------\n")) )
      (progn (insert (format "Cluster %s\n" i ))
         (do ((temp2 (car temp) (cdr temp2)))
             ((endp temp2) (insert (format "\n")))
             (progn (insert (format "Theorem %s (library %s)\n" (car (nth (1- (car temp2)) tables-thms))
                        (library-belong-thm (1- (car temp2)))))

                )
             )
         (insert (format "\n"))))
      )))



;;; Similarities for theorems

(defun print-similarities-weka-defs (res name)
  (let* ((clusters (extract-clusters-from-file-defs ))
     (temp1 (clusters-of-n clusters (nth res clusters))))
    (progn
    (with-current-buffer "*display*"
      (erase-buffer)

      (if (or (not temp1) (equal (length temp1) 1))
      (insert (format "Sorry no similarities"))
      (progn
         (insert (format "Similarities:\n"))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
      (if (equal (length temp1) 2)
      (insert (format "Definition %s is similar to definition:\n" name))
    (insert (format "Definition %s is similar to definitions:\n" name)))
      (do ((temp2 temp1 (cdr temp2)))
      ((endp temp2) )
      (if (not (string= (format "%s" (car (nth (- (car temp2)  1) tables-definitions)) )
                (format "%s" name)))
          (progn
        (insert (format "- %s (library %s)\n" (car (nth (- (car temp2)  1) tables-definitions))
                (library-belong (1- (car temp2))))))
      ))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
))))))


(defun print-similarities-weka-statement ()
  (let* ((clusters (extract-clusters-from-file-defs ))
     (temp1 (clusters-of-n clusters (nth 0 clusters))))
    (progn
    (with-current-buffer "*display*"
      (erase-buffer)

      (if (or (not temp1) (equal (length temp1) 1))
      (insert (format "Sorry no similarities"))
      (progn
         (insert (format "Similarities:\n"))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
      (if (equal (length temp1) 2)
      (insert (format "Your current goal is similar to theorem:\n" ))
    (insert (format "Your current goal is similar to theorems:\n" )))
      (do ((temp2 (cdr temp1) (cdr temp2)))
      ((endp temp2) )
    (insert (format "- %s (library %s)\n" (car (nth (- (car temp2)  1) tables-thms))
                (library-belong-thm (1- (car temp2)))))
      )
      (insert (format "------------------------------------------------------------------------------------------------\n"))
))))))




;;; Alltogether

(defun cluster-statements ()
  (interactive)
  (switch-to-display)
  (weka-thms)
  (sleep-for 2)
  (print-clusters-weka-thms (cond  ((eq 2 granularity-level) (floor (length tables-thms) 7))
                  ((eq 3 granularity-level) (floor (length tables-thms) 5))
                  ((eq 4 granularity-level) (floor (length tables-thms) 4))
                  ((eq 5 granularity-level) (floor (length tables-thms) 2))
                  (t (floor (length tables-thms) 8))))
  )



(defun member-tables-definitions (name)
  (do ((temp tables-definitions (cdr temp))
       (temp2 nil))
      ((or temp2 (endp temp)) temp2)
    (setf temp2 (or (equal name (car (car temp)))
            (equal (format "%s" name) (format "%s" (car (car temp))))))))

(defun position-tables-definitions (name)
  (do ((temp tables-definitions (cdr temp))
       (temp2 0))
      ((equal (car (car temp)) name) temp2)
    (setf temp2 (1+ temp2))))



(defun show-similarities-defs ()
  (interactive)
  (add-several-libraries-defs)
  (transform-definitions)
  ;(extract-tables-recursive)
  (let ((res (car (read-from-string (read-string "Introduce the name of the definition: ")))))
    (if (member-tables-definitions res)
    (progn  (switch-to-display)
        (weka-defs)
        (sleep-for 2)
        (print-similarities-weka-defs (position-tables-definitions res) res))
      (message "That definition has not been included"))))


(defun show-similarities-last-def ()
  (interactive)
  (add-several-libraries-defs)
  (transform-definitions)
  (switch-to-display)
  (weka-defs)
  (sleep-for 2)
  (print-similarities-weka-defs 0 (caar tables-definitions)))




(defun show-similarities-statement ()
  (interactive)
  (addcurrentgoal)
  (switch-to-display)
  (weka-thms)
  (sleep-for 2)
  (print-similarities-weka-statement )
  (setf listofstatements (cdr listofstatements))
  (setf listofthmvariables (cdr listofthmvariables)))
