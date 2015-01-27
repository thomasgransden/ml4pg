(require 'cl)

(defun extract-list (lis level res)
  (setf res (append res (list (list (car lis) (length (cdr lis)) level))))
  (do ((temp (cdr lis) (cdr temp)))
      ((endp temp) res)
    (if (listp (car temp))
    (setf res (append res (extract-list (car temp) (1+ level) nil)))
      (setf res (append res (list (list (car temp) 0 (+ 1 level))))))))

(defun quicksort-triple (list n)
  (if (<= (length list) 1)
      list
      (let ((pivot (nth n (car list))))
    (append
     (quicksort-triple (remove-if-not #'(lambda (x) (< (nth n x) pivot)) list) n)
     (remove-if-not #'(lambda (x) (= (nth n x) pivot)) list)
     (quicksort-triple (remove-if-not #'(lambda (x) (> (nth n x) pivot)) list) n)
          ))))

(defun arity_1 (formulas vars)
  (do ((temp formulas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (and (equal (nth 1 (car temp)) 0) (member (nth 0 (car temp)) vars))
    (setf temp2 (append temp2 (list (list (nth 0 (car temp))
                          -1
                          (nth 2 (car temp))))))
      (setf temp2 (append temp2 (list (car temp)))))))



(defun extract-info (thm vars)
  (let ((name (cadr thm))
    (thm1 (car (cddr thm))))
    (append (list name) (arity_1 (quicksort-triple (extract-list thm1 1 nil) 2) vars))))

(defun extract-level (formulas level)
  (do ((temp formulas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (equal (nth 2 (car temp)) level)
    (setf temp2 (append temp2 (list (car temp)))))))


(defun extract-arity (formulas arity)
  (do ((temp formulas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (equal (nth 1 (car temp)) arity)
    (setf temp2 (append temp2 (list (car temp))))))
  )

(defun build-table (list)
  (let ((name (car list))
    (formulas (cdr list)))
    (do ((i 1 (+ 1 i))
     (temp nil))
    ((equal i 8) (append (list name) temp))
      (setf temp (append temp
             (list (do ((j -1 (+ 1 j))
                    (temp2 nil))
                   ((equal j 6) temp2)
                 (setf temp2 (append temp2
                             (list (do ((temp3 (extract-arity (extract-level formulas i) j) (cdr temp3))
                                (temp4 nil))
                                   ((endp temp3) temp4)
                                 (setf temp4 (append temp4 (list (nth 0 (car temp3))))))))))))))))
