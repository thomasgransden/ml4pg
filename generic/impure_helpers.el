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
    (insert-file-contents (concat home-dir "/" type "/" name))
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

(defun write-lisp-to-file (path value)
  (with-temp-file path
    (insert (format "%s" value))))

(defun export-up-to-here-aux (dir1 val1 dir2 val2)
  (let ((name (name-from-buf)))
    (write-lisp-to-file (concat home-dir "/" dir1 "/" name) val1)
    (write-lisp-to-file (concat home-dir "/" dir2 "/" name) val2)
    t))

(defun random-elem (list)
  (when list (nth (random (length list)) list)))

;; These may be pure; they've been consolidated from a bunch of duplicates

(defun form-clusters (list n)
  (do ((i 0 (1+ i))
       (temp nil))
      ((= i n)
       temp)
    (setf temp (append temp (list (clusters-of-n list i))))))

(defun extract-clusters-from-file ()
  (extract-clusters-from-file-aux (read-lines (expand-file-name "out_bis.arff"))))

(defun extract-clusters-from-file-aux (str)
  (lines-to-clusters str))

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

(defun why-are-similar-defs ()
  (sleep-for 2)
  (let* ((file (read-lines (expand-file-name "whysimilar.txt")))
         (attributes (subseq file (+ 21 (search "Selected attributes:" file)))))
    (extract-selected-attributes (subseq attributes 0 (1- (search ":" attributes)))
                                 nil)))

(defun extract-selected-attributes (temp res)
  (let ((comma (search "," temp)))
    (if comma
        (extract-selected-attributes (subseq temp (+ 1 comma))
                                     (append res (list (car (read-from-string (subseq temp 0 comma))))))
      (append res (list (car (read-from-string temp)))))))

(defun explain-why-are-similar ()
  (let ((sim (why-are-similar)))
    (insert (format "The similarities of these lemmas are given by the following parameters:\n"))
    (do ((temp sim (cdr temp)))
        ((endp temp)
         (insert (format "------------------------------------------------------------------------------------------------\n")))
      (insert (format " - %s\n" (attribute-to-value (car temp)))))))

(defun attribute-to-value (n)
  (let* ((tdl (cond ((< n 8)  1)
                    ((< n 15) 2)
                    ((< n 22) 3)
                    ((< n 29) 4)
                    ((< n 36) 5)
                    ((< n 43) 6)
                    (t 7)))
         (arity (- (- n (* 7 (- tdl 1))) 1)))
    (if (= arity 0)
        (format "The variables of the term-tree at depth level %s" tdl)
      (format "The function(s) of arity %s of the term-tree at depth level %s"  (1- arity) tdl))))

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
        (let ((whysimilar (why-similar out)))))))

(defun weka-defs ()
  (shell-command (concat "rm " (expand-file-name "temp.csv")))
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

(defun save-numbers-aux (dir func func2)
  (interactive)
  (progn (beginning-of-buffer)
         (proof-goto-point)
         (end-of-buffer)
         (funcall func2)
         (let* ((buf (buffer-name))
                (name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
           (with-temp-file (concat home-dir "/definitions/" name)
             (insert (format "%s" listofdefinitions)))
           (with-temp-file (concat home-dir "/variables/" name)
             (insert (format "%s" listofvariables))))
         (let* ((buf (buffer-name))
                (name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
           (with-temp-file (concat home-dir "/theorems/" name)
             (insert (format "%s" listofstatements)))
           (with-temp-file (concat home-dir "/variablesthms/" name)
             (insert (format "%s" listofthmvariables))))
         (let ((d (read-string (concat "Where do you want to store this library (" (list-to-string dirs) "n (create new directory)): ")))
               (d2 nil))
           (cond ((string-member d dirs)
                  (progn (with-temp-file
                             (concat home-dir "libs/" dir "/" d "/"
                                     (subseq (buffer-name (current-buffer)) 0
                                             (search "." (buffer-name (current-buffer))))
                                     ".csv") (insert (extract-features-1)))
                         (with-temp-file
                             (concat home-dir "libs/" dir "/" d "/"
                                     (subseq (buffer-name (current-buffer)) 0
                                             (search "." (buffer-name (current-buffer))))
                                     "_tactics.csv") (insert (extract-features-2 tactic-level)))
                         (with-temp-file
                             (concat home-dir "libs/" dir "/" d "/"
                                     (subseq (buffer-name (current-buffer)) 0
                                             (search "." (buffer-name (current-buffer))))
                                     "_tree.csv") (insert (extract-features-2 proof-tree-level)))
                         (with-temp-file (concat home-dir "libs/" dir "/" d "/"
                                                 (subseq (buffer-name (current-buffer)) 0
                                                         (search "." (buffer-name (current-buffer))))
                                                 "_names") (insert (funcall func name)))))
                 ((string= d "n")
                  (progn
                    (setf d2 (read-string (concat "Introduce a name for the directory: ")))
                    (shell-command (concat "mkdir " home-dir "libs/" dir "/" d2))
                    (with-temp-file
                        (concat home-dir "libs/" dir "/" d2 "/"
                                (subseq (buffer-name (current-buffer)) 0
                                        (search "." (buffer-name (current-buffer))))
                                ".csv") (insert (extract-features-1)))
                    (with-temp-file
                        (concat home-dir "libs/" dir "/" d2 "/"
                                (subseq (buffer-name (current-buffer)) 0
                                        (search "." (buffer-name (current-buffer))))
                                "_tree.csv") (insert (extract-features-2 proof-tree-level)))
                    (with-temp-file
                        (concat home-dir "libs/" dir "/" d2 "/"
                                (subseq (buffer-name (current-buffer)) 0
                                        (search "." (buffer-name (current-buffer))))
                                "_tactics.csv") (insert (extract-features-2 tactic-level)))
                    (with-temp-file (concat home-dir "libs/" dir "/" d2 "/"
                                            (subseq (buffer-name (current-buffer)) 0
                                                    (search "." (buffer-name (current-buffer))))
                                            "_names") (insert (funcall func name)))))
                 (t
                  (progn (with-temp-file
                             (concat home-dir "libs/" dir "/"
                                     (subseq (buffer-name (current-buffer)) 0
                                             (search "." (buffer-name (current-buffer))))
                                     ".csv") (insert (extract-features-1)))
                         (with-temp-file
                             (concat home-dir "libs/" dir "/"
                                     (subseq (buffer-name (current-buffer)) 0
                                             (search "." (buffer-name (current-buffer))))
                                     "_tree.csv") (insert (extract-features-2 proof-tree-level)))
                         (with-temp-file
                             (concat home-dir "libs/" dir "/"
                                     (subseq (buffer-name (current-buffer)) 0
                                             (search "." (buffer-name (current-buffer))))
                                     "_tactics.csv") (insert (extract-features-2 tactic-level)))
                         (with-temp-file (concat home-dir "libs/" dir "/"
                                                 (subseq (buffer-name (current-buffer)) 0
                                                         (search "." (buffer-name (current-buffer))))
                                                 "_names") (insert (funcall func name)))))))))

(defun coqp (str)
  "Check whether STR contains valid Coq code by trying to compile it"
  (let* ((dir (make-temp-file "ml4pg_check_coq" t))
         (f   (concat dir "/file.v")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert str))
          (write-to-messages
           `(lambda ()
              (let ((output (process-with-cmd "coqc"
                                              ""
                                              (lambda (&rest x)
                                                (list nil (buffer-string)))
                                              ,f)))
                (message "OUTPUT:\n%s\n" output)
                (stringp output)))))
      (delete-directory dir t nil))))
