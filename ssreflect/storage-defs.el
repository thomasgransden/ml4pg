(require 'cl)

(defun export-up-to-here ()
  (interactive)
  (let ((name (name-from-buf)))
    (with-temp-file (concat home-dir "definitions/" name)
      (insert (format "%s" listofdefinitions)))
    (with-temp-file (concat home-dir "variables/" name)
      (insert (format "%s" listofvariables)))
    )
  t)

(defvar libs-defs nil)

(defun available-defs-libraries ()
  (setf libs-defs (directory-files (concat home-dir "definitions"))))

(defvar libs-statements nil)

(defun available-thm-libraries ()
  (setf libs-statements (directory-files (concat home-dir "theorems"))))

(defun import-definitions (name)
  (with-temp-buffer
    (insert-file-contents (concat home-dir "definitions/" name))
    (car (read-from-string (format "%s" (read (current-buffer)))))))

(defun import-variables (name)
  (with-temp-buffer
    (insert-file-contents (concat home-dir "variables/" name))
    (car (read-from-string (format "%s" (read (current-buffer)))))))

(defun import-statements (name)
  (with-temp-buffer
    (insert-file-contents (concat home-dir "theorems/" name))
    (car (read-from-string (format "%s" (read (current-buffer)))))))

(defun import-variablesthm (name)
  (with-temp-buffer
    (insert-file-contents (concat home-dir "variablesthms/" name))
    (car (read-from-string (format "%s" (read (current-buffer)))))))

(defvar definitions-libraries  nil)
(defvar variables-libraries    nil)
(defvar statements-libraries   nil)
(defvar variablesthm-libraries nil)

(defvar number-of-defs nil)
(defvar number-of-thms nil)

(defun add-several-libraries-defs ()
  (interactive)
  (setf definitions-libraries (reverse listofdefinitions))
  (setf number-of-defs (append number-of-defs (list (list "current" (length listofdefinitions)))))
  (available-defs-libraries)
  (setf variables-libraries (reverse listofvariables))
  (dolist (elem libs-defs)
    (let ((defs (import-definitions elem))
          (vars (import-variables elem)))
      (append-to number-of-defs   (list elem (length defs)))
      (setf definitions-libraries (append definitions-libraries defs))
      (setf variables-libraries   (append variables-libraries   vars)))))

(defvar alllibs nil)

(defun add-several-libraries-thms ()
  (interactive)
  (setf statements-libraries listofstatements)
  (setf number-of-thms (append number-of-thms (list (list "current" (length listofstatements)))))
(if alllibs
(progn  (available-thm-libraries)
  (setf variablesthms-libraries listofthmvariables)
  (do ((temp libs-statements (cdr temp)))
    ((endp temp) nil)
    (progn (setf number-of-thms (append number-of-thms (list (list (car temp) (length (import-statements (car temp)))))))
       (setf statements-libraries (append statements-libraries
                    (import-statements (car temp))))
       (setf variablesthm-libraries (append variablesthm-libraries
                    (import-variablesthm (car temp)))))))))

(defun library-belong-thm (n)
  (do ((temp number-of-thms (cdr temp))
       (temp2 nil)
       (lib "")
       (acc 0))
      (temp2 lib)
    (if (< n (+ acc (cadr (car temp))))
    (progn (setf temp2 t)
           (setf lib (car (car temp))))
      (setf acc (+ acc (cadr (car temp)))))))
