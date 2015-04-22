(require 'cl)

(defun export-up-to-here ()
  (interactive)
  (export-up-to-here-aux "definitions" listofdefinitions
                         "variables"   listofvariables))

(defvar libs-defs nil)

(defun available-defs-libraries ()
  (setf libs-defs
        (mapcar (lambda (x) (list x))
                (directory-files (concat home-dir "definitions")))))

(defvar libs-statements nil)

(defun available-thm-libraries ()
  (setf libs-statements nil)
  (shell-command  (concat "ls " home-dir "theorems |  wc -l"))
  (let ((n nil)
        (i 0))
    (with-current-buffer "*Shell Command Output*"
      (beginning-of-buffer)
      (setq n (string-to-number (format "%s"  (read (current-buffer))))))
    (shell-command  (concat "ls " home-dir "theorems"))
    (with-current-buffer "*Shell Command Output*"
      (progn (beginning-of-buffer)
             (while (< i n)
               (let ((r (format "%s" (read (current-buffer)))))
                 (progn (setq i (1+ i))
                        (setq libs-statements (append libs-statements (list r))))))))))

(defun import-definitions (name)
  (import-thing "definitions" name))

(defun import-variables (name)
  (import-thing "variables" name))

(defun import-statements (name)
  (import-thing "theorems" name))

(defun import-variablesthm (name)
  (import-thing "variablesthms" name))

(defvar definitions-libraries  nil)
(defvar variables-libraries    nil)
(defvar statements-libraries   nil)
(defvar variablesthm-libraries nil)

(defvar number-of-defs nil)
(defvar number-of-thms nil)

(defun add-several-libraries-defs ()
  (interactive)
  (add-several-libraries-aux 'definitions-libraries (reverse listofdefinitions)
                             'number-of-defs
                             'available-defs-libraries
                             'variables-libraries   (reverse listofvariables)
                             libs-defs
                             'import-definitions
                             'import-variables))

(defun add-several-libraries-thms ()
  (interactive)
  (add-several-libraries-aux 'statements-libraries           listofstatements
                             'number-of-thms
                             'available-thm-libraries
                             'variablesthm-libraries         listofthmvariables
                             libs-statements
                             'import-statements
                             'import-variablesthm))

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
