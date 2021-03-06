(defun weka (n)
  (let ((alg (cond ((string= "k" algorithm) "SimpleKMeans")
		   ((string= "e" algorithm) "EM")
		   ((string= "f" algorithm) "FarthestFirst")
	 )))
    ;(comint-send-string (get-buffer-process "*matlab*")
;		      (concat "load " (expand-file-name "temp.csv") "; [t1,X,t3] = princomp(temp); X=normalize(X); csvwrite('"
;			      (expand-file-name "temp2.csv") "',X);
;"))

    (verbose-command  (concat "sleep 1; cat " home-dir "aux_files/headers.txt " (expand-file-name "temp.csv") " > " (expand-file-name "temp3.arff")))
    (verbose-command (concat "java -classpath "
			 *weka-dir*
			 " weka.filters.unsupervised.attribute.AddCluster -W \"weka.clusterers." alg " -N " (format "%s" n) " -S 42\" -I last -i "
			 (expand-file-name "temp3.arff") " -o " (expand-file-name "out.arff")))
    (verbose-command (concat "tail -n +37 "
			   (expand-file-name "out.arff") " > " (expand-file-name "out_bis.arff")))
    (verbose-command (concat "java -classpath "
			 *weka-dir*
			 " weka.attributeSelection.CfsSubsetEval -M -s \"weka.attributeSelection.BestFirst -D 1 -N 5\" -i "
			 (expand-file-name "temp3.arff") " > " (expand-file-name "res.txt")))
  ))


(defun why-are-similar ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "res.txt"))
    (setf foo nil)
    (while (not foo)
      (setf foo (string= "attributes:" (format "%s" (read (current-buffer))))))
    (extract-selected-attributes (format "%s" (read (current-buffer))) nil))
  )


(defun extract-selected-attributes (temp res)
 (let ((comma (search "," temp)))
   (if comma
       (extract-selected-attributes (subseq temp (+ 1 comma))
				    (append res (list (car (read-from-string (subseq temp 0 comma))))))
     (append res (list (car (read-from-string temp)))))))











(defun 0_n (n)
  (do ((i 0 (1+ i))
       (temp nil))
      ((= i n) temp)
      (setf temp (append temp (list (list i nil))))))


(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string
     (buffer-string) "\n" t)
    ))


(defun lines-to-clusters (lines)
  (do ((temp lines (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (setf temp2 (append temp2 (list (string-to-number (subseq (car temp) (+ 7 (search "cluster" (car temp) :from-end t)))))))
      ))



(defun extract-clusters-from-file (clusters)
  (let* ((temp (0_n clusters))
	 (lines (read-lines (expand-file-name "out_bis.arff"))))
    (lines-to-clusters lines)))





(defun form-clusters (list n)
  (do ((i 0 (1+ i))
       (temp nil))
      ((= i n) temp)
      (setf temp (append temp (list (clusters-of-n list i))))))




(defun clusters-of-n (list n)
  (do ((temp list (cdr temp))
       (i 1 (1+ i))
       (temp2 nil))
      ((endp temp) temp2)
      (if (equal (car temp) n)
	  (setf temp2 (append temp2 (list i))))))


(defun remove-alone (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (if (not (= (length (car temp)) 1))
	  (setf temp2 (append temp2 (list (car temp)))))))
