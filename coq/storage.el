;; FIXME: we're saving to home-dir/*.txt but reading from home-dir/coq/*.txt

(defun save-lemma-aux (string)
  (append-to-file string nil (concat home-dir "lemmas.txt")))

(defun save-lemma (name value)
  (save-lemma-aux (format "%s&%s$" name value)))

(defun save-view-aux (string)
  (append-to-file string nil (concat home-dir "views.txt")))

(defun save-view (name value)
  (save-view-aux (format "%s&%s$" name value)))

(defun read-lemmas ()
  (let ((f (concat home-dir "coq/lemmas.txt")))
    (if (file-exists-p f)
        (with-temp-buffer
          (insert-file-contents f)
          (let ((temp (format "%s" (read (current-buffer)))))
            (setf theorems_id (extract-info-from-files temp)))))))

(defun init-lemmas ()
  (if (equal init 0)
      (progn (read-lemmas)
             (setq init 1))))

(defun read-views ()
  (let ((f (concat home-dir "coq/views.txt")))
    (if (file-exists-p f)
        (with-temp-buffer
          (insert-file-contents f)
          (let ((temp (format "%s" (read (current-buffer)))))
            (setf views_id (extract-info-from-files temp)))))))

(defun extract-info-from-files (string)
  (do ((temp string)
       (temp2 nil))
      ((not (search "$" temp))
       temp2)
    (let ((dollar (search "$" temp))
          (amper  (search "&" temp)))
      (progn
        (setf temp2 (append temp2 (list (cons (str-to temp amper)
                                              (string-to-number (str-between temp "&" "$"))))))
        (setf temp  (subseq temp (1+ dollar)))))))
