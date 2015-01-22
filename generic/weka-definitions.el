(defun switch-to-display ()
  (if noninteractive (set-buffer (get-buffer-create "*display*"))
                     (switch-to-buffer-other-window "*display*")))

(defun cluster-definitions ()
  (interactive)
  (switch-to-display)
  (weka-defs)
  (sleep-for 2)
  (print-clusters-weka-defs (cond  ((eq 2 granularity-level)
                                    (floor (length tables-definitions) 7))
                                   ((eq 3 granularity-level)
                                    (floor (length tables-definitions) 5))
                                   ((eq 4 granularity-level)
                                    (floor (length tables-definitions) 4))
                                   ((eq 5 granularity-level)
                                    (floor (length tables-definitions) 2))
                                   (t (floor (length tables-definitions) 8)))))

(defun weka-thms ()
  (let ((alg (cond ((string= "k" algorithm) "SimpleKMeans")
                   ((string= "e" algorithm) "EM")
                   ((string= "f" algorithm) "FarthestFirst")))
        (n 0))
    (shell-command (concat "rm " (expand-file-name "temp.csv")))
    (with-temp-file (expand-file-name "temp.csv")
      (insert (convert-all-thms-to-weka-format-several)))
    (setf n (cond  ((eq 2 granularity-level) (floor (length tables-thms) 7))
                   ((eq 3 granularity-level) (floor (length tables-thms) 5))
                   ((eq 4 granularity-level) (floor (length tables-thms) 4))
                   ((eq 5 granularity-level) (floor (length tables-thms) 2))
                   (t (floor (length tables-thms) 8))))

    (shell-command  (concat "sleep 1; cat " home-dir "aux_files/headersdefs.txt "
                            (expand-file-name "temp.csv")
                            " > "
                            (expand-file-name "temp3.arff")))
    (shell-command (concat "sleep 1; java -classpath "
                           *weka-dir*
                           " weka.filters.unsupervised.attribute.AddCluster -W \"weka.clusterers." alg " -N " (format "%s" n)
                           " -S 42\" -I last -i "
                           (expand-file-name "temp3.arff")
                           " -o " (expand-file-name "out.arff")))
    (shell-command (concat "tail -n +56 "
                           (expand-file-name "out.arff")
                           " > " (expand-file-name "out_bis.arff")))

    (if whysimilar
        (shell-command (concat "java -classpath "
                               *weka-dir*
                               " weka.attributeSelection.InfoGainAttributeEval -s \"weka.attributeSelection.Ranker -T 0 -N 5\" -i "
                               (expand-file-name "out.arff")
                               " > " (expand-file-name "whysimilar.txt"))))))

(defun weka (n)
  (let ((alg (cond ((string= "k" algorithm) "SimpleKMeans")
                   ((string= "e" algorithm) "EM")
                   ((string= "f" algorithm) "FarthestFirst"))))
    (shell-command  (concat "sleep 1; cat " home-dir "aux_files/headers.txt " (expand-file-name "temp.csv") " > " (expand-file-name "temp3.arff")))
    (shell-command (concat "java -classpath "
                           *weka-dir*
                           " weka.filters.unsupervised.attribute.AddCluster -W \"weka.clusterers." alg " -N " (format "%s" n) " -S 42\" -I last -i "
                           (expand-file-name "temp3.arff") " -o " (expand-file-name "out.arff")))
    (shell-command (concat "tail -n +37 "
                           (expand-file-name "out.arff") " > " (expand-file-name "out_bis.arff")))
    (shell-command (concat "java -classpath "
                           *weka-dir*
                           " weka.attributeSelection.CfsSubsetEval -M -s \"weka.attributeSelection.BestFirst -D 1 -N 5\" -i "
                           (expand-file-name "temp3.arff") " > " (expand-file-name "res.txt")))))

(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string
     (buffer-string) "\n" t)))
