(defmacro append-to (name val)
  `(setf ,name (append ,name (list ,val))))

(defmacro cons-prepend (name val)
  `(setf ,name (cons ,val ,name)))

(defmacro concat-to (name lst)
  `(setf ,name (concat ,name ,lst)))

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
  (library-belong-aux (n number-of-defs)))

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

(defun extract-clusters-from-file (clusters)
  (let* ((temp (0_n clusters))
         (lines (read-lines (expand-file-name "out_bis.arff"))))
    (lines-to-clusters lines)))

(defun extract-clusters-from-file-defs ()
  (let* ((lines (read-lines1 (expand-file-name "out_bis.arff"))))
    (lines-to-clusters lines)))

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
  (do ((i 0 (1+ i))
       (temp nil))
      ((= i n) temp)
    (setf temp (append temp (list (list i nil))))))

(defun read-lines1 (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string
     (buffer-string)
     "\n" t)))

(defun lines-to-clusters (lines)
  (do ((temp lines (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (setf temp2 (append temp2 (list (string-to-number (subseq (car temp) (+ 7 (search "cluster" (car temp) :from-end t)))))))
    ))
