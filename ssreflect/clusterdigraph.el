;;++++++++++++++++++++++++++++++++++++++++++++++++++
;; Generating graphs about clusters
;;++++++++++++++++++++++++++++++++++++++++++++++++++

;;--------------------------------------------------
;; Cluster graph definitions
;;--------------------------------------------------

(defvar clustercounter 0)

(defun show-diagram-clusters (text)
  "Render the given dot code into a HTML file and open it"
  (let ((path (make-temp-file "ml4pg-clusters" nil ".html")))
    (with-temp-file path
        (insert (show-diagram-clusters-aux text)))
    (shell-command (concat "xdg-open " path))))

(defun showclustergraph (lol)
  (show-diagram-clusters (clusterofseveral-pure lol
                                                tables-definitions
                                                number-of-defs)))

(defun dependencygraph-defs-aux ()
  (subclustersseveral (dependencygraph-defs-get-cluster 2 5)
                      (dependencygraph-defs-get-cluster 5 3)))

(defun dependencygraph-defs-get-cluster (divisor granularity-level)
  (dependencygraph-get-cluster-aux granularity-level
                                   divisor
                                   'weka-defs
                                   tables-definitions))

(defun dependencygraph-get-cluster-aux2 (granularity-level divisor func size)
  (cdr (form-clusters (extract-clusters-from-file-aux (funcall func))
                      (floor size divisor))))

(defun dependencygraph-get-cluster-aux (granularity-level divisor func tbl)
  (dependencygraph-get-cluster-aux2 granularity-level
                                    divisor
                                    func
                                    (length tbl)))

(defun dependencygraph-defs ()
  (interactive)
  (showclustergraph (dependencygraph-defs-aux)))

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
  (showclustergraph-statements (dependencygraph-statements-aux)))

(defun dependencygraph-statements-aux ()
  (subclustersseveral (dependencygraph-statements-cluster 5 2)
                      (dependencygraph-statements-cluster 3 5)))


(defun dependencygraph-statements-cluster (granularity-level divisor)
  (dependencygraph-get-cluster-aux granularity-level
                                   divisor
                                   'weka-thms
                                   tables-thms))

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
  (showclustergraph-proof (dependencygraph-proof-aux)))

(defun dependencygraph-proof-writetmp-aux ()
  (if libs-menus
      (let ((s1 (cond ((string= level "g") (extract-features-1))
                      ((string= level "t") (extract-features-2 tactic-level))
                      ((string= level "p") (extract-features-2 proof-tree-level))))
            (s2 (add-libraries-notemp)))
        (add-names)
        (concat s1 s2))
      (extract-features-1)))

(defun dependencygraph-proof-aux ()
  (interactive)
  (let* ((temp      (dependencygraph-proof-writetmp-aux))
         (size      (size-notemp temp))
         (clusters3 nil)
         (clusters1 (dependencygraph-get-cluster-aux2
                     3
                     5
                     `(lambda () (weka-notemp (floor ,size 5) ,temp))
                     size))
         (clusters3 (dependencygraph-get-cluster-aux2
                     5
                     2
                     `(lambda () (weka-notemp (floor ,size 2) ,temp))
                     size)))
    (subclustersseveral (removenil (remove-if-empty clusters3))
                        (removenil (remove-if-empty clusters1)))))

(defun remove-if-empty-aux (lol thms)
  (let (result1)
    (dolist (elem1 lol result1)
      (let (result2)
        (dolist (elem2 elem1 (setf result1 (cons result2 result1)))
          (unless (string= "" (car (nth (1- elem2) thms)))
            (setf result2 (cons elem2 result2))))))))

(defun remove-if-empty (lol)
  (remove-if-empty-aux lol saved-theorems))
