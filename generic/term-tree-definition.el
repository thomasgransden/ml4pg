(defvar listofdefinitions nil)
(defvar listofvariables nil)
(defvar tables-definitions nil)

(defun obtain-definition (name)
  "Obtain definition"
  (message "FIXME: Stop trying to parse output intended for humans!")
  (send-coq-cmd (format "Print %s" name)))

(defun adddefinition (name)
  (interactive)
  (send-coq-cmd (format "Unset Printing Notations."))
  (let* ((iftable  (send-coq-cmd (format "Print Table Printing If.")))
         (coqprint `(lambda (action)
                      (unless (search "None" ,iftable)
                        (send-coq-cmd (format "%s Printing If %s."
                                              action
                                              (subseq ,iftable
                                                      (1+ (search ":" ,iftable))))))))
         (term     nil))
    (funcall coqprint "Remove")
    (setf term (obtain-definition name))
    (append-to listofdefinitions (list 'definition
                                       (make-symbol name)
                                       (car (definition-to-list (car (clean-term term))))))
    (append-to listofvariables (cadr (definition-to-list (car (clean-term term)))))
    (funcall coqprint "Add")
    (send-coq-cmd (format "Set Printing Notations."))))

(defun transform-definitions ()
  (setf tables-definitions nil)
  (do ((temp definitions-libraries (cdr temp))
       (temp2 variables-libraries (cdr temp2)))
      ((endp temp) nil)
    (append-to tables-definitions
               (build-table (extract-info (car temp) (car temp2))))))

(defun definition-to-list (term)
  (cond ((search "fix " term)
         (list (definition-to-list-fix term) (definition-variables term)))

        ((search "let " term)
         (list (definition-to-list-let term) nil))

        ((search "fun " term)
         (list (definition-to-list-fun term) (definition-variables term)))

        (t
         (list (definition-to-list-aux term) nil))))

(defun definition-variables (term)
  (car (read-from-string (concat "("
                                 (cond ((and (search "fun " term)
                                             (search "fix " term))
                                        (variables-fun-fix (subseq term 0 (search ":=" term))))

                                       ((search "fun" term)
                                        (variables-fun (subseq term (+ 3 (search "fun" term))
                                                               (search "=>" term))))

                                       ((search "fix" term)
                                        (variables-fix (subseq term (+ 3 (search "fix" term))
                                                               (search ":=" term)))))
                                 ")"))))

(defun variables-fun-fix (term)
  (concat (variables-fun (subseq term (+ 3 (search "fun" term))
                                 (search "=>" term)))
          " "
          (variables-fix (subseq term (+ 3 (search "fix" term))))))

(defun variables-fix (term)
  (do ((posop (search "(" term))
       (temp0 term)
       (temp2 ""))
      ((not posop) temp2)
    (setf temp2 (concat temp2 (subseq temp0 (1+ posop) (search ":" temp0))))
    (setf temp0 (subseq temp0 (1+ (search ":" temp0))))
    (setf posop (search "(" temp0))))

(defun remove-argument (string)
  (subseq string 0 (search "Argument" string)))

(defun clean-term (term)
  "Clean a term"
  (let* ((clean-term (remove-argument (remove-whitespaces (remove-jumps term))))
         (obj        (subseq clean-term 0 (search ":" clean-term :from-end t)))
         (type       (subseq clean-term (1+ (search ":" clean-term :from-end t)))))
    (list obj type)))

(defun add-parenthesis-match (term)
  (message "FIXME: is add-parenthesis-match just replace-regexp-in-string?")
  (do ((temp0 term)
       (ift   (search "match" term))
       (temp2 ""))
      ((not ift) (concat temp2 temp0))
    (setf temp2 (concat temp2 (subseq temp0 0 ift) "match ("))
    (setf temp0 (subseq temp0 (+ 5 ift)))
    (setf ift (search "match" temp0))))

(defun remove-with (term)
  (message "FIXME: is remove-with just replace-regexp-in-string?")
  (do ((temp0 term)
       (ift   (search " with" term))
       (temp2 ""))
      ((not ift) (concat temp2 temp0))
    (setf temp2 (concat temp2 (subseq temp0 0 ift)))
    (setf temp0 (subseq temp0 (+ 5 ift)))
    (setf ift   (search " with" temp0))))

(defun remove-end (term)
  (message "FIXME: is remove-end just replace-regexp-in-string?")
  (do ((temp0 term)
       (ift   (search "end" term))
       (temp2 ""))
      ((not ift) (concat temp2 temp0))
    (setf temp2 (concat temp2 (subseq temp0 0 ift) ")"))
    (setf temp0 (subseq temp0 (+ 3 ift)))
    (setf ift   (search "end" temp0))))

(defun remove-arrow (term)
  (message "FIXME: is remove-arrow just replace-regexp-in-string?")
  (do ((temp0 term)
       (ift   (search ">" term))
       (temp2 ""))
      ((not ift) (concat temp2 temp0))
    (setf temp2 (concat temp2 (subseq temp0 0 ift) "("))
    (setf temp0 (subseq temp0 (+ 1 ift)))
    (setf ift (search ">" temp0))))


(defun remove-bar (term)
  (if (search "|" term)
      (let ((bar (search "|"   term :from-end))
            (arr (search "=> " term :from-end)))
        (when (or (not arr) (< arr bar))
          (error "remove-bar requires '=> ' after '|' in '%s'" term))
        ;; Start computation
        (do ((post term)
             (ift  (search "|" term))
             (pre  ""))
            ((not ift) (concat pre post))
          (setf pre (concat pre (subseq post 0 ift) ")"))
          (setf post (subseq post  (1+ (search "=> " post))))
          (setf ift (search "|" post))))
    term))

(defun transform-length-1 (list)
  (do ((temp  list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (append-to temp2 (if (consp (car temp))
                         (if (equal (length (car temp)) 1)
                             (caar temp)
                           (transform-length-1 (car temp)))
                       (car temp)))))

(defun replace-quote (term)
  (do ((temp0 term)
       (ift   (search "'" term))
       (temp2 ""))
      ((not ift) (concat temp2 temp0))
    (setf temp2 (concat temp2 (subseq temp0 0 ift) "quo"))
    (setf temp0 (subseq temp0 (1+ ift)))
    (setf ift (search "'" temp0))))

(defun add-parentheses-match0 (term)
  (concat "("
          (replace-quote
           (remove-arrow
            (remove-bar
             (add-parenthesis-match
              (remove-end
               (remove-with term))))))
          ")"))

(defun transform-match (term)
  (transform-length-1 (car (read-from-string (add-parentheses-match0 term)))))

(defun ml4pg-string-to-list (str)
  (car (read-from-string str)))

(defun definition-to-list-aux (term)
  (unless (search "=" term) (error "No '=' found in '%s'" term))
  (ml4pg-string-to-list (concat "(" (subseq term (1+ (search "=" term))) ")")))

(defun definition-to-list-let (term)
  (ml4pg-string-to-list (concat "(nosimpl "
                                (subseq term (+ 3 (search "in" term)))
                                ")")))

(defun  definition-to-list-fix (term)
  (transform-match (subseq term (+ 2 (search ":=" term)))))

(defun  definition-to-list-fun (term)
  (if (search "match" term)
      (transform-match (subseq term (+ 2 (search "=>" term))))
    (ml4pg-string-to-list (concat "("
                                  (subseq term (+ 2 (search "=>" term)))
                                  ")"))))

(defun variables-fun (term)
  (do ((posop (search "(" term))
       (temp0 term)
       (temp2 ""))
      ((not posop) (if (search ":" temp0) (subseq temp0 0 (search ":" temp0)) temp2))
    (setf temp2 (concat temp2 (subseq temp0 (1+ posop) (search ":" temp0))))
    (setf temp0 (subseq temp0 (1+ (search ":" temp0))))
    (setf posop (search "(" temp0))))

(defun remove-jumps (string)
  (message "WARNING: using the remove-jumps from generic/term-tree-definition.el")
  (message "FIXME: is remove-jumps just replace-regexp-in-string?")
  (do ((temp   string)
       (result "")
       (jump   (search "\n" string)))
      ((not jump) (concat result temp))
    (setf result (concat result (subseq temp 0 jump) " "))
    (setf temp   (subseq temp (1+ jump)))
    (setf jump   (search "\n" temp))))
