;; Pure functions taken from feature_extractionv2.el

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in))

(defun first-space (txt)
  "Find the position of the first space in a string"
  (search " " txt))

(defun after-space (txt)
  "Find the position of text after the first space"
  (1+ (first-space txt)))

(defun first-dot (txt)
  "Find the position of the first dot in a string"
  (search "." txt))

(defun pos-to-dot (cmd n)
  "Extract a sub-string from the given position to the first dot"
  (subseq cmd n (first-dot cmd)))

(defun between-spaces (txt)
  (let ((space (after-space txt)))
    (subseq txt space (search " " txt :start2 space))))

(defun rem-jumps (cmd)
  (remove-jumps (between-spaces cmd)))

(defun str-after (str pattern)
  (subseq str (+ (length pattern) (search pattern str))))

(defun goal-str-aux (s)
  (str-after s "============================\n   "))

(defun get-type-id-aux (txt)
  (flet ((txt-pos (begin end)
                  (search begin txt :start2 (+ 2 (search end txt)))))
    (subseq txt (+ 2 (search ": " txt))
            (or (txt-pos " " ": ")
                (txt-pos nl  " ")))))

(defun get-top-symbol-aux (goal)
  (let* ((fst-symbol (subseq goal 0 (first-space goal))))
    (get-top-symbol-num fst-symbol goal)))

(defun get-top-symbol-num (fst-symbol goal)
  (cond ((string= "forall" fst-symbol) 5)
        ((search  "->"     goal)       7)
        ((string= "@eq"    fst-symbol) 6)
        ((string= "and"    fst-symbol) 4)
        ((string= "iff"    fst-symbol) 8)
        ((string= "or"     fst-symbol) 3)
        (0)))

(defun extract-params (seq res)
  (extract-params-aux nl seq res))

(defun extract-params2 (seq res)
  (extract-params-aux "." seq res))

(defun extract-params-aux (sep seq res)
  (let ((pos_space (first-space seq))
        (pos_jump  (search sep seq)))
    (if pos_space
        (extract-params-aux sep
                            (subseq seq (1+ pos_space))
                            (cons (subseq seq 0 pos_space) res))
      (reverse (cons (subseq seq 0 pos_jump) res)))))

(defun get-types-list-aux (f list res)
  (if (endp list)
      (* -1 res)
    (get-types-list-aux (cdr list)
                        (+ (* -1 (apply f (list (car list)))
                              (expt 10 (1- (length list))))
                           res))))

(defun get-number-list (list)
  "Obtain the number of tactics applied"
  (if (endp list)
      0
    (+ (expt 10 (1- (length list)))
       (get-number-list (cdr list)))))

(defun get-top-symbols-list-aux (top intro len res)
  (if (= len 0)
      res
    (let ((gs (funcall top))
          (ps (funcall intro)))
      (+ (get-top-symbols-list-aux top
                                   intro
                                   (1- len)
                                   (+ (* gs (expt 10 (1- len)))
                                      res))))))

(defun get-top-symbols-seq-aux (top intro seq res)
  (if (endp seq)
      res
    (let ((gs (funcall top)))
      (apply intro (car seq))
      (+ (get-top-symbols-seq-aux (cdr seq)
                                  (+ (* gs (expt 10 (1- (length seq))))
                                     res))))))

(defun search-in-hyp (obj hyp)
  "Obtain the value associated with a theorem"
  (unless (endp hyp)
    (if (string= obj (car hyp))
        t
      (search-in-hyp obj (cdr hyp)))))

(defun arg-induction-aux (object res)
  (if (search "Error" res) -1 1))

(defun remove-dots (str)
  (replace-regexp-in-string (regexp-quote ".") "" str))

(defun remove-nonalpha (str)
  (replace-regexp-in-string "[^a-z]" "" str))

(defun get-obj-intro-aux (obj)
  (subseq obj 0 (search nl obj)))

(defun extract-theorem-id-aux (cmd)
  (let ((s<- (or (search "<-" cmd) 0)))
    (pos-to-dot cmd (if s<- (+ 3 s<-)
                      (after-space cmd)))))

(defun starts-with (str prefix)
  (string= (subseq str 0
                   (min (length prefix)
                        (length str)))
           prefix))
