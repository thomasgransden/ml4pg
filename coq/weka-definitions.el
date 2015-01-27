;;; Weka invokation

(defvar whysimilar nil)

(defun convert-all-thms-to-weka-format-several ()
  (setf thms-vectors nil)
  (add-several-libraries-thms)
  (transform-thms)
  (convert-recursive-several-libraries-thms)
  (do ((temp (last-part-of-lists thms-vectors) (cdr temp))
     (temp2 ""))
    ((endp temp) temp2)
    (setf temp2 (concat temp2 (format "%s\n"  (print-list  (car temp) ))))))

;;; Printing clusters


(defun print-clusters-weka-defs (gra out_bis)
  (let* ((clusters (extract-clusters-from-file-aux out_bis))
     (res1 (remove-nil (remove-alone (cdr (form-clusters clusters gra))))))
    (with-current-buffer "*display*"
      (erase-buffer)
      (insert (format "We have found the following clusters:\n" ))
      (insert (format "------------------------------------------------------------------------------------------------\n" ))

      (do ((temp res1 (cdr temp))
       (i 1 (1+ i)))
      ((endp temp) (insert (format "------------------------------------------------------------------------------------------------\n")) )
      (progn (insert (format "Cluster %s\n" i ))
         (do ((temp2 (car temp) (cdr temp2)))
             ((endp temp2) (insert (format "\n")))
             (progn (insert (format "Definition %s (library %s)\n" (car (nth (1- (car temp2)) tables-definitions))
                        (library-belong (1- (car temp2)))))))
         (insert (format "\n")))))))

(defun print-clusters-weka-thms (gra out_bis)
  (let* ((clusters (extract-clusters-from-file-aux out_bis))
     (res1 (remove-nil (remove-alone (cdr (form-clusters clusters gra))))))
    (with-current-buffer "*display*"
      (erase-buffer)
      (insert (format "We have found the following clusters:\n" ))
      (insert (format "------------------------------------------------------------------------------------------------\n" ))

      (do ((temp res1 (cdr temp))
       (i 1 (1+ i)))
      ((endp temp) (insert (format "------------------------------------------------------------------------------------------------\n")) )
      (progn (insert (format "Cluster %s\n" i ))
         (do ((temp2 (car temp) (cdr temp2)))
             ((endp temp2) (insert (format "\n")))
             (progn (insert (format "Theorem %s (library %s)\n" (car (nth (1- (car temp2)) tables-thms))
                        (library-belong-thm (1- (car temp2)))))))
         (insert (format "\n")))))))
