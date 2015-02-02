;; This function starts Matlab

(defvar my-buffer "")

(defvar signal 0
  "The variable signal is used to indicate the function which has called to matlab and to process the result")

(defun split-clusters-aux (str res)
  (let ((init (search "ans =" str)))
    (if init
        (let ((end (search "[" str :start2 (1+ init))))
          (split-clusters-aux (subseq str (1+ end))
                              (cons (cluster-string-to-list (remove-jumps (subseq str (+ 5 init)
                                                                                  end)))
                                    res)))
      res)))


(defun split-frequencies (str res)
  (let ((init (search "[" str)))
    (if init
        (let ((end (search "]" str :start2 (1+ init))))
          (if (not (search "char" (subseq str init end)))
              (split-frequencies (subseq str (1+ end))
                                 (cons (string-to-number (remove-jumps (subseq str (1+ init) end))) res))
            (split-frequencies (subseq str (1+ (search "[" str :start2 (1+ end))))  res)))
      res)))

(defun remove-jumps (str)
  (do ((temp str)
       (temp2 "")
       (i 0 (1+ i))
       (jump (search "\n" str)))
      ((not jump)
       (if (= i 0)
           str temp2))
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp 0 jump)))
           (setf temp (subseq temp (1+ jump)))
           (setf jump (search "\n" temp)))))

(defun cluster-string-to-list (cluster)
  (do ((temp cluster)
       (temp2 nil))
      ((not (search "," temp))
       (append temp2 (list temp)))
    (progn (setf temp2 (append temp2 (list (subseq temp 0 (search "," temp)))))
           (setf temp (subseq temp (1+ (search "," temp)))))))

(defun remove-occurrence (list n)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp)
       temp2)
    (if (not (equal (format "%s" n)
                    (car temp)))
        (setf temp2 (append temp2 (list (car temp)))))))

(defun print-similarities (res)
  (print-similarities-aux (lambda (&rest args)) res))

(defun insert-button-lemma (lemma)
  (progn (insert-button lemma 'action (insert-button-lemma-macro lemma)
                        'face (list 'link)
                        'follow-link t)))

(defun insert-button-lemma-macro (test)
  (list 'lambda '(x)
        (list 'progn
              (list 'send-coq-cmd (list 'format '"Unset Printing All."))
              (list 'if (list 'get-buffer '"*display2*")
                    (list 'with-current-buffer '"*display2*" (list 'delete-window)))
              (list 'with-current-buffer '"*display*" (list 'split-window-vertically))
              (list 'switch-to-buffer-other-window '"*display2*")
              (list 'with-current-buffer '"*display2*" (list 'erase-buffer))
              (list 'with-current-buffer '"*display2*"
                    (list 'insert (list 'send-coq-cmd
                                        (list 'format '"Print %s." test)))))))

(defvar times 0)

(defun print-clusters (res freq)
  (interactive)
  (setf times (1+ times))
  (if (not (caar res))
      (insert (format "Searching clusters...\n"))
    (let* ((temp0 (unzip (quicksort-pair (zip res freq))))
           (res1 (car temp0))
           (freq1 (cadr  temp0)))
      (insert (format "We have found the following clusters:\n" ))
      (insert (format "------------------------------------------------------------------------------------------------------------\n"))
      (do ((temp res1 (cdr temp))
           (temp-freq freq1 (cdr temp-freq))
           (i 1 (1+ i)))
          ((endp temp)
           (insert (format "------------------------------------------------------------------------------------------------------------\n"))
           )
        (progn (insert (format "Cluster %s with frequency %s%%\n" i (car temp-freq)))

               (do ((temp2 (car temp)
                           (cdr temp2)))
                   ((endp temp2)
                    (insert (format "\n")))
                 (progn (insert (format "Lemma "))
                        (insert-button-lemma
                         (remove_last_colon
                          (car (nth (string-to-number (car temp2))
                                    saved-theorems)))))))))))

(defun print-clusters-weka (gra str)
  (print-clusters-weka-aux gra 'identity str))

(defun print-clusters-weka-aux (gra func1 str)
  (let* ((clusters (extract-clusters-from-file-aux str))
         (res1     (funcall func1 (remove-alone (cdr (form-clusters clusters gra))))))
    (with-current-buffer "*display*"
      (erase-buffer)
      (insert "We have found the following clusters:\n")
      (insert "-------------------------------------------------------------------------------------\n")

      (dotimes (j (length res1) (insert "-------------------------------------------------------------------------------------\n"))
        (let ((i    (1+ j))
              (elems (nth j res1)))
          (insert (format "Cluster %s: (" i))
          (ignore-errors (insert-button-automaton2 (which-lemmas-in-cluster elems) elems))
          (insert ")\n")
          (dolist (elem elems (insert "\n"))
            (ignore-errors
              (if (<= elem (length saved-theorems))
                  (progn (insert "Lemma ")
                         (insert-button-lemma (remove_last_colon
                                               (remove-jumps (car (nth (1- elem) saved-theorems)))))
                         (insert (format " (%s)\n" (which-patch (1- elem) 1))))
                  (progn (print-clusters-weka-namecmd)
                         (insert "Lemma ")
                         (unless (search "home" temp-res)
                           (insert (format "%s\n" temp-res))))))))))))

(defun which-patch (n m)
  (cond ((equal n 0)
         "first patch")
        ((and (not (equal (car (nth n saved-theorems))
                          (car (nth (- n 1)
                                    saved-theorems))))
              (not (equal (car (nth n saved-theorems))
                          (car (nth (+ n 1)
                                    saved-theorems)))))
         "unique patch")
        ((and (equal (car (nth n saved-theorems))
                     (car (nth (- n 1)
                               saved-theorems)))
              (not (equal (car (nth n saved-theorems))
                          (car (nth (+ n 1)
                                    saved-theorems)))))
         "last patch")
        ((equal (car (nth n saved-theorems))
                (car (nth (- n 1)
                          saved-theorems)))

         (which-patch (1- n)
                      (1+ m)))
        (t (format "patch %s" m))))

(defun which-lemmas-in-cluster (l)
  (do ((temp l (cdr temp))
       (res nil))
      ((endp temp) res)
    (if (<= (car temp) (length saved-theorems))
        (setf res (append res (list (remove_last_colon
                                     (remove-jumps (car (nth (1- (car temp))
                                                             saved-theorems)))))))
      (progn (shell-command (concat "cat "(expand-file-name "names_temp.txt") " | sed -n '"
                                    (format "%s" (- (car temp) (length saved-theorems)))
                                    "p'"))
             (with-current-buffer "*Shell Command Output*"
               (beginning-of-buffer)
               (read (current-buffer))
               (setf temp-res (format "%s" (read (current-buffer)))))
             (if (not (search "home" temp-res))
                 (setf res (append res (list temp-res))))))))

(defun insert-button-automaton (l)
  (progn (insert-button "automaton" 'action (insert-button-automaton-macro (list 'quote l))
                        'face (list 'link)
                        'follow-link t)))

(defun insert-button-automaton-macro (l)
  (list 'lambda '(x)
        (list 'generate-automaton l)))

(defun remove_last_colon (str)
  (if (string= (subseq str (1- (length str)))
               ":")
      (subseq str 0 (1- (length str)))
    str))

(defun show-clusters-alg (str)
  (if (string= "g" str) "find_cluster_with_gaussian"
                        "find_cluster_with_kmeans"))

(defun show-clusters-of-theorem-data (res)
  (cond ((string= level "g") (extract-features-1-bis res))
        ((string= level "t") (extract-features-2-bis tactic-temp tactic-level))
        ((string= level "p") (extract-features-2-bis proof-tree-temp proof-tree-level))))

(defun show-clusters-of-theorem ()
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
      (setf saved-theorems-libs (mapcar 'cadr saved-theorems))
      (switch-to-display)
      (setf signal 5)
      (let* ((arg (floor size
                         (case granularity-level
                           (2 7)
                           (3 5)
                           (4 4)
                           (5 2)
                           (t 8))))
             (out_bis (weka-notemp arg tmp)))
        (print-similarities-weka-str arg out_bis))))
  (send-coq-cmd (format "Unset Printing All")))

(defun show-clusters ())

(defun show-clusters-bis ()
  (interactive)
  (setf saved-theorems (remove-nil-cases))
  (setf buf (current-buffer))
  (let* ((alg (cond ((string= "g" algorithm)
                     "gaussian_clusters")
                    (t "kmeans_clusters_and_frequencies")))
         (gra (case granularity-level
                (2 5)
                (3 10)
                (4 15)
                (5 20)
                (t 3)))
         (freq (case frequency-precision
                 (2 500)
                 (3 1000)
                 (t 100))))

    (setf signal 4)
    (setf my-buffer "")
    (let ((temp (dependencygraph-proof-writetmp-aux)))
      (setf saved-theorems-libs (mapcar (lambda (x)
                                          (cadr x))
                                        saved-theorems))
      (switch-to-display)
      (setf signal 5)
      (let ((lvl (floor (size-temp) (case granularity-level
                                      (2 7)
                                      (3 5)
                                      (4 4)
                                      (5 2)
                                      (t 8)))))
        (print-clusters-weka lvl (weka-notemp lvl temp))))
    (send-coq-cmd "Unset Printing All")))

(defvar saved-theorems-libs nil)

(defun add-libraries ()
  (do ((temp libs-menus (cdr temp)))
      ((endp temp)
       nil)
    (cond ((string= level "g")
           (progn
             (shell-command  (concat "cat " home-dir "libs/coq/" (car temp)
                                     ".csv >> " (expand-file-name "temp1.csv")))
             (add-to-saved-theorems-libs (concat home-dir "libs/coq/" (car temp)
                                                 ".csv"))))
          ((string= level "t")
           (shell-command  (concat "cat " home-dir "libs/coq/" (car temp)
                                   "_tactics.csv >> " (expand-file-name "temp1.csv"))))
          ((string= level "p")
           (shell-command  (concat "cat " home-dir "libs/coq/" (car temp)
                                   "_tree.csv >> " (expand-file-name "temp1.csv")))))))

(defun add-libraries-temp ()
  (add-libraries-temp-aux "coq" t))

(defun add-libraries-notemp ()
  (add-libraries-temp-str "coq" t))

(defun add-names ()
  (add-names-aux "coq"))

(defvar names-values nil)

(defvar granularity-dynamic 0)

(defun show-clusters-dynamic ())

(defun show-clusters-dynamic-b ())
