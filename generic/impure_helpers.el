(defmacro append-to (name val)
  `(setf ,name (append ,name (list ,val))))

(defmacro cons-prepend (name val)
  `(setf ,name (cons ,val ,name)))

(defmacro concat-to (name lst)
  `(setf ,name (concat ,name ,lst)))

(defun process-with-cmd (cmd stdin &rest args)
  "Run command 'cmd', with string 'stdin' as its stdin. Additional arguments for
   'cmd' can be supplied after 'stdin'. Returns the stdout as a string."
  (with-temp-buffer
    (insert stdin)
    (apply 'call-process-region (append (list (point-min)
                                              (point-max)
                                              cmd
                                              t
                                              t
                                              nil)
                                        args))
    (buffer-string)))

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
  (do ((temp number-of-defs (cdr temp))
       (temp2 nil)
       (lib "")
       (acc 0))
      (temp2 lib)
    (if (< n (+ acc (cadr (car temp))))
        (progn (setf temp2 t)
               (setf lib (car (car temp))))
      (setf acc (+ acc (cadr (car temp)))))))

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
