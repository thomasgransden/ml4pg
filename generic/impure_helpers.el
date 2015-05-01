(defmacro append-to (name val)
  `(setf ,name (append ,name (list ,val))))

(defmacro cons-prepend (name val)
  `(setf ,name (cons ,val ,name)))

(defmacro concat-to (name lst)
  `(setf ,name (concat ,name ,lst)))

(defun read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun process-with-cmd (cmd stdin &optional handler &rest args)
  "Run command CMD, with string STDIN as its stdin. ARGS can contain additional
   arguments for CMD. Returns the stdout as a string. If the exist code is
   nonzero, it will be passed to HANDLER. If HANDLER is nil, an error occurs."
  (with-temp-buffer
    (insert stdin)
    (let ((code (apply 'call-process-region (append (list (point-min)
                                                          (point-max)
                                                          cmd
                                                          t
                                                          t
                                                          nil)
                                                    args))))
      (if (equal 0 code)
          (buffer-string)
          (if handler (funcall handler code)
                      (error "Command %s failed with code %s" cmd code))))))

(defun import-thing (type name)
  (with-temp-buffer
    (insert-file-contents (concat home-dir type "/" name))
    (car (read-from-string (format "%s" (read (current-buffer)))))))

(defun add-several-libraries-aux (libs1 list1
                                  number1
                                  available1
                                  libs2 vals2
                                  loop-init
                                  importer1
                                  importer2)
  "Set up a bunch of variables, using a bunch of functions"
  (set libs1 list1)
  (set number1 (append (symbol-value number1)
                       (list (list "current" (length list1)))))
  (funcall available1)
  (set libs2 vals2)
  (do ((temp loop-init (cdr temp)))
      ((endp temp) nil)
    (let* ((elem (car temp))
           (defs (funcall importer elem)))
      (set number1 (append (symbol-value number1)
                           (list elem (length defs))))
      (set libs1   (append (symbol-value libs1) defs))
      (set libs2   (append (symbol-value libs2)
                           (funcall import-variables elem))))))

(defun library-belong (n)
  (library-belong-aux n number-of-defs))

(defun name-from-buf ()
  (let ((buf (buffer-name)))
    (if (search "." buf)
        (subseq buf 0 (search "." buf))
      buf)))

(defun random-elem (list)
  (when list (nth (random (length list)) list)))

;; These may be pure; they've been consolidated from a bunch of duplicates

(defun form-clusters (list n)
  (do ((i 0 (1+ i))
       (temp nil))
      ((= i n)
       temp)
    (setf temp (append temp (list (clusters-of-n list i))))))

(defun extract-clusters-from-file-aux (str)
  (lines-to-clusters (string-split str "\n")))

(defun last-part-of-lists (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp)
       temp2)
    (setf temp2 (append temp2 (list (cadar temp))))))

(defun convert-all-definitions-to-weka-format-several ()
  (add-several-libraries-defs)
  (transform-definitions)
  (convert-recursive-several-libraries-defs)
  (do ((temp (last-part-of-lists defs-vectors)
             (cdr temp))
       (temp2 ""))
      ((endp temp)
       temp2)
    (setf temp2 (concat temp2 (format "%s\n"  (print-list  (car temp)))))))

(defun why-are-similar ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "res.txt"))
    (setf foo nil)
    (while (not foo)
      (setf foo (string= "attributes:" (format "%s" (read (current-buffer))))))
    (extract-selected-attributes (format "%s" (read (current-buffer))) nil)))

(defun extract-selected-attributes (temp res)
  (let ((comma (search "," temp)))
    (if comma
        (extract-selected-attributes (subseq temp (+ 1 comma))
                                     (append res (list (car (read-from-string (subseq temp 0 comma))))))
      (append res (list (car (read-from-string temp)))))))

(defun 0_n (n)
  (let (temp)
    (dotimes (i n temp)
      (setf temp (append temp (list (list i nil)))))))

(defun lines-to-clusters (lines)
  (let (result)
    (dolist (elem lines result)
      (when (search "cluster" elem :from-end t)
        (append-to result
                   (string-to-number (subseq elem
                                             (+ 7 (search "cluster"
                                                          elem
                                                          :from-end t)))))))))

(defun weka-defs-cmd (data alg n)
  (process-with-cmd "java" data nil
                    "-classpath" *weka-dir*
                    "weka.filters.unsupervised.attribute.AddCluster"
                    "-W" (concat "weka.clusterers." alg
                                 " -N " (format "%s" n)
                                 " -S 42")
                    "-I" "last"))

(defun weka-defs-aux (algorithm)
  (weka-defs-aux-aux algorithm
                     'convert-all-definitions-to-weka-format-several
                     tables-definitions))

(defun weka-defs-aux-aux (algorithm convert tbl)
  (let* ((alg     (weka-alg algorithm))
         (n       0)
         (temp3   (append-headers (funcall convert)))
         (n       (weka-defs-n granularity-level tbl))
         (out     (weka-defs-cmd temp3 alg n))
         (out_bis (process-with-cmd "tail" out nil
                                    "-n" "+56")))
    (if whysimilar
        (let ((whysimilar (why-similar out)))
          (write-whysimilar whysimilar)))
    out_bis))

(defun write-whysimilar (str)
  (with-temp-file (expand-file-name "whysimilar.txt")
    (insert str)))

(defun weka-defs ()
  (ignore-errors (delete-file (expand-file-name "temp.csv")))
  (weka-defs-aux algorithm))

(defun weka-defs-n (gl td)
  (let ((d (case gl
             (2 7)
             (3 5)
             (4 4)
             (5 2)
             (t 8))))
    (floor (length td) d)))

(defun remove-nil (l)
  (do ((temp l (cdr temp))
       (res nil))
      ((endp temp) res)
    (if (not (endp (car temp)))
        (setf res (append res (list (car temp)))))))

(defun zip (l1 l2)
  (do ((temp1 l1 (cdr temp1))
       (temp2 l2 (cdr temp2))
       (res nil))
      ((endp temp1) res)
    (setf res (append res (list (append (list (car temp1)) (list (car temp2))))))))

(defun unzip (l)
  (do ((temp l (cdr temp))
       (res1 nil)
       (res2 nil))
      ((endp temp) (list (reverse res1) (reverse res2)))
    (progn (setf res1 (cons (caar temp) res1))
           (setf res2 (cons (cadr (car temp)) res2)))))

(defun quicksort-pair (list)
  (if (<= (length list) 1)
      list
    (let ((pivot (cadar list)))
      (append (quicksort-pair (remove-if-not #'(lambda (x) (> (cadr x) pivot)) list))
              (remove-if-not #'(lambda (x) (= (cadr x) pivot)) list)
              (quicksort-pair (remove-if-not #'(lambda (x) (< (cadr x) pivot)) list))))))

(defun save-numbers-aux-aux (name)
  (dolist (elem (list (list "definitions"   listofdefinitions)
                      (list "variables"     listofvariables)
                      (list "theorems"      listofstatements)
                      (list "variablesthms" listofthmvariables)))
    (with-temp-file (concat home-dir (car elem) "/" name)
      (insert (format "%s" (cadr elem))))))

(defun save-numbers-aux (dir func func2)
  (let ((d     (read-string (concat "Where do you want to store this library ("
                                    (list-to-string dirs)
                                    "n (create new directory)): ")))
        (mkdir nil)
        (d2    (cond
                ((string-member d dirs)
                 (concat d "/"))

                ((string= d "n")
                 (setq mkdir t)
                 (concat (read-string "Introduce a name for the directory: ") "/"))

                (t ""))))
    (save-numbers-mkdir dir func func2 d2 mkdir)))

(defun save-numbers-mkdir (dir func func2 d mkdir)
  (when mkdir
    (shell-command (concat "mkdir " home-dir "libs/" dir "/" d)))
  (save-numbers-noask dir func func2 d))

(defun save-numbers-noask (dir func func2 d)
  (interactive)
  (goto-char (point-min))
  (unless (proof-locked-region-empty-p)
    (proof-retract-until-point-interactive))
  (goto-char (point-max))
  (funcall func2)
  (save-numbers-aux-aux (name-from-buf))

  (dolist (elem (list (list ".csv"         (lambda (x) (extract-features-1)))
                      (list "_tree.csv"    (lambda (x) (extract-features-2 proof-tree-level)))
                      (list "_tactics.csv" (lambda (x) (extract-features-2 tactic-level)))
                      (list "_names"       func)))
    (with-temp-file
        (concat home-dir "libs/" dir "/" d (name-from-buf) (car elem))
      (insert (funcall (cadr elem) name)))))

(defun coqp (str)
  (let ((res (coqp-aux str)))
    (unless res
      (message "Not valid Coq code: %s" str))
    res))

(defun coqp-aux (str)
  "Check whether STR contains valid Coq code by trying to compile it"
  (let* ((dir (make-temp-file "ml4pg_check_coq" t))
         (f   (concat dir "/file.v")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert str))
          (write-to-messages
           `(lambda ()
              (stringp (process-with-cmd "coqc"
                                         ""
                                         (lambda (&rest x)
                                           (list nil (buffer-string)))
                                         ,f)))))
      (delete-directory dir t nil))))

(defun send-coq-cmd (str)
  ;(message "SENDING: %s" str)
  (let ((result (proof-shell-invisible-cmd-get-result str)))
    ;(message "GOT: %s" result)
    result))

(defun exported-libraries ()
  (interactive)
  (unless noninteractive
    (easy-menu-remove-item nil '("Statistics") "Show cluster libraries")
    (easy-menu-add-item nil
                        '("Statistics")
                        (cons "Available libraries for clustering:"
                              (cons ["Current" nil
                                     :selected t
                                     :style toggle
                                     :help "Use the current library for clustering"]
                                    (select-libraries))))))

(defun step-over-proof ()
  "Step forward by one command; if it wasn't 'Proof.', step back again."
  (let* ((start (proof-queue-or-locked-end))
         (foo   (proof-assert-next-command-interactive))
         (end   (proof-queue-or-locked-end))
         (cmd   (remove-whitespace
                 (buffer-substring-no-properties start end))))
    (unless (equal "Proof." cmd)
      (message "FIXME: Can we avoid waiting for the prover?")
      (goto-char start)
      (sit-for 0.5)
      (proof-goto-point)
      (sit-for 0.5))))

(defun choose-distinct (size)
  "Generates a random number of distinct random numbers, up to SIZE, in order.
   SIZE must be >= 1. Always returns at least one number. Never returns more
   than SIZE numbers, since there wouldn't be enough distinct numbers <= SIZE!"
  (when (< size 1)
    (error "Can't choose from %s" size))
  ;; Choose a random number of partitions; from between 1 partition of length
  ;; SIZE, to SIZE partitions of length 1
  (let ((n      (1+ (random size)))
        (result nil))
    (dotimes (i n result)
      ;; Choose a random number from 1 to (SIZE - i), since i possibilities have
      ;; already been taken
      (let ((x (1+ (random (- size i)))))
        (append-to result
                   ;; Step over any preceding choices
                   (dolist (prev result x)
                     (when (<= prev x)
                       (setq x (1+ x)))))
        (sort result '<)))))

(defun choose-partitions (size)
  "Choose random positive numbers which sum to SIZE"
  (let ((result nil)  ;; Resulting list of numbers
        (prev   0))   ;; The previous number we saw
    (dolist (elem
             ;; Loop over some distinct random numbers, in order, less than SIZE
             (sort (choose-distinct size) '<))
      ;; Keep the distances between the random numbers
      (append-to result (- elem prev))
      (setq prev elem))
    ;; Treat any remainder as another chunk
    (when (< prev size)
      (append-to result (- size prev)))
    result))
