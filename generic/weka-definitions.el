(defun switch-to-display ()
  (if noninteractive (set-buffer (get-buffer-create "*display*"))
                     (switch-to-buffer-other-window "*display*")))

(defun cluster-definitions ()
  (interactive)
  (switch-to-display)
  (let ((out_bis (weka-defs)))
    (print-clusters-weka-defs (weka-defs-n granularity-level tables-definitions)
                              out_bis)))

(defun weka-alg (a)
  (cond ((string= "k" a) "SimpleKMeans")
        ((string= "e" a) "EM")
        ((string= "f" a) "FarthestFirst")))

(defun append-headers (data)
  (concat (read-file (concat home-dir "aux_files/headersdefs.txt")) data))

(defun weka-thms ()
  (weka-defs-aux-aux algorithm
                     'convert-all-thms-to-weka-format-several
                     tables-thms))

(defun why-similar (data)
  (process-with-cmd "java" data nil
                    "-classpath" *weka-dir*
                    "weka.attributeSelection.InfoGainAttributeEval"
                    "-s" "weka.attributeSelection.Ranker -T 0 -N 5"))

(defun weka (n)
  (weka-notemp n (read-file (expand-file-name "temp.csv"))))

(defun weka-notemp (n temp)
  (let* ((alg     (weka-alg algorithm))
         (headers (read-file (concat home-dir "aux_files/headers.txt")))
         (temp3   (concat headers temp))
         (out     (process-with-cmd "java" temp3 nil
                                    "-classpath" *weka-dir*
                                    "weka.filters.unsupervised.attribute.AddCluster"
                                    " -W" (concat "weka.clusterers." alg
                                                  " -N " (format "%s" n)
                                                  " -S 42")
                                    "-I" "last"))
         (out_bis (process-with-cmd "tail" out nil
                                    "-n" "+37"))
         (res     (process-with-cmd "java" temp3 nil
                                    "-classpath" *weka-dir*
                                    "weka.attributeSelection.CfsSubsetEval"
                                    "-M"
                                    "-s" "weka.attributeSelection.BestFirst -D 1 -N 5")))
    (write-res res)
    out_bis))

(defun write-res (str)
  (with-temp-file (expand-file-name "res.txt")
    (insert str)))

(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun cluster-statements ()
  (interactive)
  (switch-to-display)
  (let ((out_bis (weka-thms)))
    (print-clusters-weka-thms (floor (length tables-thms) (case granularity-level
                                                            (2 7)
                                                            (3 5)
                                                            (4 4)
                                                            (5 2)
                                                            (t 8)))
                              out_bis)))

(defun print-similarities-weka-defs (res name out_bis)
  (let* ((clusters (extract-clusters-from-file-aux out_bis))
         (temp1 (clusters-of-n clusters (nth res clusters))))
    (progn
      (with-current-buffer "*display*"
        (erase-buffer)

        (if (or (not temp1)
                (equal (length temp1) 1))
            (insert (format "Sorry no similarities"))
          (progn
            (insert (format "Similarities:\n"))
            (insert single-line)
            (if (equal (length temp1) 2)
                (insert (format "Definition %s is similar to definition:\n" name))
              (insert (format "Definition %s is similar to definitions:\n" name)))
            (do ((temp2 temp1 (cdr temp2)))
                ((endp temp2))
              (if (not (string= (format "%s" (car (nth (- (car temp2)  1) tables-definitions)))
                                (format "%s" name)))
                  (progn
                    (insert (format "- %s (library %s)\n" (car (nth (- (car temp2)  1) tables-definitions))
                                    (library-belong (1- (car temp2))))))))
            (insert single-line)))))))

(defun show-similarities-last-def ()
  (interactive)
  (add-several-libraries-defs)
  (transform-definitions)
  (switch-to-display)
  (let ((out_bis (weka-defs)))
    (print-similarities-weka-defs 0
                                  (caar tables-definitions)
                                  out_bis)))

(defun print-similarities-weka-statement (out_bis)
  (let* ((clusters (extract-clusters-from-file-aux out_bis))
         (temp1 (clusters-of-n clusters (nth 0 clusters))))
    (progn
      (with-current-buffer "*display*"
        (erase-buffer)

        (if (or (not temp1) (equal (length temp1) 1))
            (insert "Sorry no similarities")
          (progn
            (insert "Similarities:\n")
            (insert single-line)
            (if (equal (length temp1) 2)
                (insert "Your current goal is similar to theorem:\n" )
              (insert "Your current goal is similar to theorems:\n" ))
            (do ((temp2 (cdr temp1) (cdr temp2)))
                ((endp temp2))
              (insert (format "- %s (library %s)\n" (car (nth (- (car temp2) 1) tables-thms))
                              (library-belong-thm (1- (car temp2))))))
            (insert single-line)))))))

(defun show-similarities-statement ()
  (interactive)
  (addcurrentgoal)
  (switch-to-display)
  (let ((out_bis (weka-thms)))
    (print-similarities-weka-statement out_bis))
  (setf listofstatements (cdr listofstatements))
  (setf listofthmvariables (cdr listofthmvariables)))
