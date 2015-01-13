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
