(defun weka (n)
  (let ((alg (cond ((string= "k" algorithm) "SimpleKMeans")
           ((string= "e" algorithm) "EM")
           ((string= "f" algorithm) "FarthestFirst")
     )))
    ;(comint-send-string (get-buffer-process "*matlab*")
;             (concat "load " (expand-file-name "temp.csv") "; [t1,X,t3] = princomp(temp); X=normalize(X); csvwrite('"
;                 (expand-file-name "temp2.csv") "',X);
;"))

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
             (expand-file-name "temp3.arff") " > " (expand-file-name "res.txt"))) ))

(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string
     (buffer-string) "\n" t)
    ))
