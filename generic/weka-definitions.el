(defun switch-to-display ()
  (if noninteractive (set-buffer (get-buffer-create "*display*"))
                     (switch-to-buffer-other-window "*display*")))

(defun cluster-definitions ()
  (interactive)
  (switch-to-display)
  (weka-defs)
  (sleep-for 2)
  (print-clusters-weka-defs (weka-defs-n granularity-level tables-definitions)))

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
  (let* ((alg     (weka-alg algorithm))
         (headers (read-file (concat home-dir "aux_files/headers.txt")))
         (temp    (read-file (expand-file-name "temp.csv")))
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
                                    "-s" "weka.attributeSelection.BestFirst -D 1 -N 5")))))

(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))
