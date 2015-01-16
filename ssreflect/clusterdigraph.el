;;++++++++++++++++++++++++++++++++++++++++++++++++++
;; Generating graphs about clusters
;;++++++++++++++++++++++++++++++++++++++++++++++++++

;;--------------------------------------------------
;; Cluster graph definitions
;;--------------------------------------------------

(defvar clustercounter 0)

(defun clusterofseveral (lol)
  (clusterofseveral-pure lol tables-definitions number-of-defs))

(defun clusterofseveral-pure (lol tbl defs)
  (clusterofseveral-aux (clusterofone lol 1 tbl defs)))

(defun show-diagram-clusters-aux (text)
  "Turns the given dot code into a PNG with an image map, and return a HTML
   page containing this image"
  (let* ((map (process-with-cmd "dot" text "-Tcmap"))
         (png (process-with-cmd "dot" text "-Tpng"))
         (b64 (process-with-cmd "base64" png)))
    (createwebpage map b64)))

(defun show-diagram-clusters (text)
  "Render the given dot code into a HTML file and open it"
  (let ((path (make-temp-file "ml4pg-clusters" nil ".html")))
    (with-temp-file path
        (insert (show-diagram-clusters-aux text)))
    (shell-command (concat "xdg-open " path))))

(defun showclustergraph (lol)
  (showclustergraph-pure lol tables-definitions number-of-defs))

(defun showclustergraph-pure (lol tbl defs)
  (show-diagram-clusters (clusterofseveral-pure lol tbl defs)))

(defun replacecluster (cluster1 cluster2)
  (if (endp cluster2)
      (list cluster1)
    (if (listp (car cluster2))
        (if (issubcluster cluster1 (car cluster2))
            (cons (replacecluster cluster1 (car cluster2))
                  (cdr cluster2))
          (cons (car cluster2)
                (replacecluster cluster1 (cdr cluster2))))
      (if (member (car cluster2)
                  cluster1)
          (replacecluster cluster1 (cdr cluster2))
        (cons (car cluster2)
              (replacecluster cluster1 (cdr cluster2)))))))

(defun subclusters (cluster clusters)
  (do ((temp clusters (cdr temp))
       (flag nil)
       (flag1 nil)
       (res nil))
      ((or (endp temp)
           flag flag1)
       (cond (flag (append (reverse res)
                           temp))
             (flag1 clusters)
             (t (append (reverse res)
                        (list cluster)))))
    (cond ((not (issubcluster cluster (car temp)))
           (setf res (cons (car temp)
                           res)))
          ((equal cluster (car temp))
           (setf res (reverse clusters))
           (setf flag1 t))

          (t
           (setf res (cons (replacecluster cluster (car temp))
                           res))
           (setf flag t)))))

(defun subclustersseveral (clusters1 clusters2)
  (do ((temp clusters1 (cdr temp))
       (temp2 clusters2))
      ((endp temp)
       temp2)
    (setf temp2 (subclusters (car temp)
                             temp2))))

(defun dependencygraph-defs ()
  (interactive)
  (let ((clusters1 nil)
        (clusters3 nil))
    (setf granularity-level 3)
    (weka-defs)
    (sleep-for 2)
    (setf clusters1 (cdr (form-clusters (extract-clusters-from-file-defs )
                                        (floor (length tables-definitions)
                                               5))))
    (setf granularity-level 5)
    (weka-defs)
    (sleep-for 2)
    (setf clusters3 (cdr (form-clusters (extract-clusters-from-file-defs )
                                        (floor (length tables-definitions)
                                               2))))
    (showclustergraph (subclustersseveral  clusters3 clusters1 ))))

;;--------------------------------------------------
;; Cluster graph lemma statements
;;--------------------------------------------------

(defun clusterofone-statements (lst)
  (do ((temp lst (cdr temp))
       (res ""))
      ((endp temp)
       res)
    (if (listp (car temp))
        (progn (setf clustercounter (1+ clustercounter))
               (setf res (concat res (format "subgraph cluster%s {\n" clustercounter)
                                 (clusterofone-statements (car temp))
                                 "\n}\n")))
      (if (and (cdr temp)
               (not (listp (cadr temp))))
          (setf res (concat res
                            (format "%s [URL=\"./%s.html#%s\"]; %s -> %s[style=invis]\n" (car (nth (1- (car temp))
                                                                                                   tables-thms))
                                    (library-belong-thm (1- (car temp)))
                                    (car (nth (1- (car temp))
                                              tables-thms))
                                    (car (nth (1- (car temp))
                                              tables-thms))
                                    (car (nth (1- (cadr temp))
                                              tables-thms))
                                    )))
        (setf res (concat res
                          (format "%s [URL=\"./%s.html\"];\n" (car (nth (1- (car temp))
                                                                        tables-thms))
                                  (library-belong-thm (1- (car temp))))))))))

(defun clusterofseveral-statements (lol)
  (setf clustercounter 0)
  (concat "digraph {\n rankdir=LR;\n" (clusterofone-statements lol)
          "\n}"))

(defun showclustergraph-statements (lol)
  (show-diagram-clusters (clusterofseveral-statements lol)))

(defun dependencygraph-statements ()
  (interactive)
  (let ((clusters1 nil)
        (clusters3 nil))
    (setf granularity-level 3)
    (weka-thms)
    (sleep-for 2)
    (setf clusters1 (cdr (form-clusters (extract-clusters-from-file-defs )
                                        (floor (length tables-thms)
                                               5))))
    (setf granularity-level 5)
    (weka-thms)
    (sleep-for 2)
    (setf clusters3 (cdr (form-clusters (extract-clusters-from-file-defs )
                                        (floor (length tables-thms)
                                               2))))
    (showclustergraph-statements (subclustersseveral  clusters3 clusters1 ))))

;;--------------------------------------------------
;; Cluster graph proofs
;;--------------------------------------------------

(defun clusterofone-proof (lst)
  (do ((temp lst (cdr temp))
       (res ""))
      ((endp temp)
       res)
    (if (listp (car temp))
        (progn (setf clustercounter (1+ clustercounter))
               (setf res (concat res (format "subgraph cluster%s {\n" clustercounter)
                                 (clusterofone-proof (car temp))
                                 "\n}\n")))
      (if (and (cdr temp)
               (not (listp (cadr temp))))
          (let ((thm nil)
                (thm2 nil))
            (if (<= (car temp)
                    (length saved-theorems))
                (setf thm (car (nth (1- (car temp))
                                    saved-theorems)))
              (progn (shell-command (concat "cat "(expand-file-name "names_temp.txt")
                                            " | sed -n '"
                                            (format "%s" (- (car temp)
                                                            (length saved-theorems)))
                                            "p'"))
                     (with-current-buffer "*Shell Command Output*"
                       (beginning-of-buffer)
                       (read (current-buffer))
                       (setf thm (format "%s"  (read (current-buffer)))))))
            (if (<= (car temp)
                    (length saved-theorems))
                (setf thm2 (car (nth (1- (cadr temp))
                                     saved-theorems)))
              (progn (shell-command (concat "cat "(expand-file-name "names_temp.txt")
                                            " | sed -n '"
                                            (format "%s" (- (cadr temp)
                                                            (length saved-theorems)))
                                            "p'"))
                     (with-current-buffer "*Shell Command Output*"
                       (beginning-of-buffer)
                       (read (current-buffer))
                       (setf thm2 (format "%s"  (read (current-buffer)))))))
            (setf res (concat res
                              (format "%s; %s -> %s[style=invis]\n"
                                      thm
                                      thm
                                      thm2))))
        (let ((thm nil))
          (if (<= (car temp)
                  (length saved-theorems))
              (setf thm (car (nth (1- (car temp))
                                  saved-theorems)))
            (progn (shell-command (concat "cat "(expand-file-name "names_temp.txt")
                                          " | sed -n '"
                                          (format "%s" (- (car temp)
                                                          (length saved-theorems)))
                                          "p'"))
                   (with-current-buffer "*Shell Command Output*"
                     (beginning-of-buffer)
                     (read (current-buffer))
                     (setf thm (format "%s"  (read (current-buffer)))))))
          (setf res (concat res (format "%s;\n" thm))))))))

(defun clusterofseveral-proof (lol)
  (setf clustercounter 0)
  (concat "digraph {\n rankdir=LR;\n" (clusterofone-proof lol)
          "\n}"))

(defun showclustergraph-proof (lol)
  (show-diagram-clusters (clusterofseveral-proof lol)))

(defun dependencygraph-proof ()
  (interactive)
  (if libs-menus
      (progn (with-temp-file (expand-file-name "temp.csv")
               (cond ((string= level "g")
                      (insert (extract-features-1)))
                     ((string= level "t")
                      (insert (extract-features-2 tactic-level)))
                     ((string= level "p")
                      (insert (extract-features-2 proof-tree-level)))))
             (add-libraries-temp)
             (add-names))
    (with-temp-file (expand-file-name "temp.csv")
      (insert (extract-features-1))))
  (let ((clusters1 nil)
        (clusters3 nil))
    (setf granularity-level 3)
    (weka (floor (size-temp)
                 5))
    (sleep-for 2)
    (setf clusters1 (cdr (form-clusters (extract-clusters-from-file (floor (size-temp)
                                                                           5))
                                        (floor (size-temp)
                                               5))))
    (setf granularity-level 5)
    (weka (floor (size-temp)
                 2))
    (sleep-for 2)
    (setf clusters3 (cdr (form-clusters (extract-clusters-from-file (floor (size-temp)
                                                                           2))
                                        (floor (size-temp)
                                               2))))
    (showclustergraph-proof (subclustersseveral (removenil (remove-if-empty clusters3))
                                                (removenil (remove-if-empty clusters1))))))

(defun remove-if-empty-aux (lol thms)
  (let (result1)
    (dolist (elem1 lol result1)
      (let (result2)
        (dolist (elem2 elem1 (setf result1 (cons result2 result1)))
          (unless (string= "" (car (nth (1- elem2) thms)))
            (setf result2 (cons elem2 result2))))))))

(defun remove-if-empty (lol)
  (remove-if-empty lol saved-theorems))
