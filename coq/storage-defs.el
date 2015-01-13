(require 'cl)

(defun name-from-buf ()
  (let ((buf (buffer-name)))
    (if (search "." buf)
        (subseq buf 0 (search "." buf))
      buf)))

(defun export-up-to-here-aux (dir1 vals1 dir2 vals2)
  (let ((name (name-from-buf)))
    (with-temp-file (concat home-dir "/" dir1 "/" name)
      (insert (format "%s" vals1)))

    (with-temp-file (concat home-dir "/" dir2 "/" name)
      (insert (format "%s" vals2)))
    t))

(defun export-library-aux (action)
  (interactive)
  (beginning-of-buffer)
  (proof-goto-point)
  (end-of-buffer)
  (extract-feature-theorems)
  (funcall action))

(defun export-library-defs ()
  (interactive)
  (export-library-aux 'export-up-to-here))

(defun export-library-thms ()
  (interactive)
  (export-library-aux 'export-up-to-here-thm))

(defun export-up-to-here ()
  (interactive)
  (export-up-to-here-aux "definitions" listofdefinitions
                         "variables"   listofvariables))

(defun export-up-to-here-thm ()
  (interactive)
  (export-up-to-here-aux "theorems"      listofstatements
                         "variablesthms" listofthmvariables))

(defvar libs-defs nil)

(defun available-defs-libraries ()
  (setf libs-defs nil)
  (shell-command  (concat "ls " home-dir "definitions |  wc -l"))
  (let ((n nil)
        (i 0))
    (with-current-buffer "*Shell Command Output*"
      (beginning-of-buffer)
      (setq n (string-to-number (format "%s"  (read (current-buffer))))))
    (shell-command  (concat "ls " home-dir "definitions"))
    (with-current-buffer "*Shell Command Output*"
      (progn (beginning-of-buffer)
             (while (< i n)
               (let ((r (format "%s" (read (current-buffer)))))
                 (progn (setq i (1+ i))
                        (setq libs-defs (append libs-defs (list r))))))))))

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

(defun import-thing (type name)
  (with-temp-buffer
    (insert-file-contents (concat home-dir "/" type "/" name))
    (car (read-from-string (format "%s" (read (current-buffer)))))))

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
  (setf definitions-libraries (reverse listofdefinitions))
  (append-to number-of-defs (list "current" (length listofdefinitions)))
  (available-defs-libraries)
  (setf variables-libraries (reverse listofvariables))
  (do ((temp libs-defs (cdr temp)))
      ((endp temp) nil)
    (let* ((elem (car temp))
           (defs (import-definitions elem)))
      (append-to number-of-defs (list elem (length defs)))

      (setf definitions-libraries (append definitions-libraries defs))
      (setf variables-libraries (append variables-libraries (import-variables elem))))))

(defun add-several-libraries-thms ()
  (interactive)
  (setf statements-libraries listofstatements)
  (setf number-of-thms (append number-of-thms (list (list "current" (length listofstatements)))))
  (available-thm-libraries)
  (setf variablesthms-libraries listofthmvariables)
  (do ((temp libs-statements (cdr temp)))
      ((endp temp)
       nil)
    (progn (setf number-of-thms (append number-of-thms (list (list (car temp)
                                                                   (length (import-statements (car temp)))))))
           (setf statements-libraries (append statements-libraries
                                              (import-statements (car temp))))
           (setf variablesthm-libraries (append variablesthm-libraries
                                                (import-variablesthm (car temp)))))))

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
