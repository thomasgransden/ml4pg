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

(defun print-clusters-weka (gra str)
  (print-clusters-weka-aux gra 'remove-nil str))

(defun print-clusters-weka-aux (gra func1 str)
  (let* ((clusters (extract-clusters-from-file-aux str))
         (res1     (funcall func1 (remove-alone (cdr (form-clusters clusters gra))))))
    (with-current-buffer "*display*"
      (erase-buffer)
      (insert "We have found the following clusters:\n")
      (insert single-line)

      (dotimes (j (length res1) (insert single-line))
        (let ((i     (1+ j))
              (elems (nth j res1)))
          (unless (equal elems '(nil))
              (progn
                (insert (format "Cluster %s: (" i))
                (ignore-errors (insert-button-automaton2 (which-lemmas-in-cluster elems) elems))
                (insert ")\n")
                (dolist (elem elems (insert "\n"))
                  (ignore-errors
                    (if (< elem (length saved-theorems))
                        (progn (insert "Lemma ")
                               (insert-button-lemma (remove_last_colon
                                                     (car (nth elem saved-theorems))))
                               (insert (format " (%s)\n" (which-patch elem 1))))
                      (progn (print-clusters-weka-namecmd)
                             (insert "Lemma " )
                             (unless (search "home" temp-res)
                                 (insert-button-lemma temp-res))
                             (insert "\n")))))))))
      (insert "\n======================================================================================\n")
      (insert explain-why-sim)
      (insert "\n======================================================================================\n"))))

(defun explain-why-sim ()
  (let ((result "The similarities of the clusters are given by the following parameters:\n")))
  (dolist (elem (why-are-similar) nil)
    (let ((nth-level (case (floor elem 6)
                       (0 "1st")
                       (1 "2nd")
                       (2 "3rd")
                       (3 "4th")
                       (4 "5th"))))
      (concat-to result (format (case (mod elem 6)
                                  (0 " - The 1st tactic (or tactics) applied in the %s goal of the proof.\n")
                                  (1 " - The number of tactics applied in the %s goal of the proof.\n")
                                  (2 " - The type of the arguments of the tactic (or tactics) applied in the\n   %s goal of the proof.\n")
                                  (3 " - The proof step of the %s goal of the proof is linked to a hypothesis,\n   inductive hypothesis or library lemmas.\n")
                                  (4 " - The top symbol of the %s goal of the proof.\n")
                                  (5 " - The number of subgoals generated after the %s step of the proof.\n"))
                                nth-level)))))

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
        (setf res (append res (list (remove-jumps (car (nth (car temp) saved-theorems))) )))
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

(defun show-clusters-of-theorem ()
  (show-clusters-of-theorem-aux (lambda (size gra) gra)
                                (lambda ())))

(defun show-clusters-bis ()
  (interactive)
  (let* ((alg  (if (string= "g" algorithm) "gaussian_clusters"
                                           "kmeans_clusters_and_frequencies"))
         (n    0)
         (gra  (case granularity-level
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
  (setf buf (current-buffer))
  (let ((temp (dependencygraph-proof-writetmp-aux)))
    (switch-to-display)
    (setq n (size-notemp temp))
    (setf gra (floor n (case granularity-level
                         (2 7)
                         (3 5)
                         (4 4)
                         (5 2)
                         (t 8))))
    (setf signal 5)
    (print-clusters-weka gra (weka-notemp gra temp))))
  (send-coq-cmd (format "Unset Printing All")))

(defun add-libraries-notemp ()
  (add-libraries-temp-str "ssreflect" nil))

(defun add-names ()
  (add-names-aux "ssreflect"))

(defvar names-values nil)

(defvar granularity-dynamic 0)
