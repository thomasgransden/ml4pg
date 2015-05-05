;; Pure functions taken from feature_extractionv2.el

(defun goal-str-aux (s)
  (if s (str-after s "============================\n   ")))

(defun get-type-id-aux (txt)
  (let ((txt-pos `(lambda (begin end)
                    (search begin ,txt :start2 (+ 2 (search end ,txt))))))
    (subseq txt (+ 2 (search ": " txt))
            (or (funcall txt-pos " " ": ")
                (funcall txt-pos nl  " ")))))

(defun get-top-symbol-aux (goal)
  (if goal
      (let ((fst-symbol (subseq goal 0 (first-space goal))))
        (get-top-symbol-num fst-symbol goal))))

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

(defun get-types-list-aux (f list res)
  (if (endp list)
      (* -1 res)
    (get-types-list-aux f
                        (cdr list)
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

(defun arg-induction-aux (res)
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

(defun get-numbers-get-object (cmd)
  (subseq cmd (after-space cmd) (first-dot cmd)))

(defun replace-colon (str)
  (let ((colon (search ";" str)))
    (if colon
        (replace-colon (concatenate 'string (subseq str 0 colon) "+" (subseq str (+ 2 colon))))
      str)))

(defun replace-plus (str)
  (let ((colon (search "+" str)))
    (if colon
        (replace-plus (concatenate 'string (subseq str 0 colon) "; " (subseq str (1+ colon))))
      str)))

(defun is-intro (cmd)
  (or (starts-with cmd "intros [")
      (starts-with cmd "intros;")
      (starts-with cmd "intros until")
      (search ";intros" cmd)))

(defun generate-zeros (n)
  (let (res)
    (dotimes (i n res)
      (setq res (cons 0 res)))))

(defun print-list (list)
  (let ((str ""))
    (dolist (elem list (if (string= str "")
                           ""
                           (subseq str 0 (1- (length str)))))
      (concat-to str (format "%s," (cond ((equal elem  1.0e+INF)  100)
                                         ((equal elem -1.0e+INF) -100)
                                         (t                      elem)))))))

(defun extract-features-2 (list)
  (do ((feature-list list (cdr feature-list))
       (features     ""))
      ((endp feature-list) features)
    (concat-to features (format "%s\n" (print-list (car feature-list))))))

(defun remove-last-col (str)
  (if (string= (subseq str (1- (length str))) ":")
      (subseq str 0 (1- (length str)))
    str))

(defun list-of-commands (str)
  (do ((temp (subseq str 0 (1- (length str))))
       (i 0 (1+ i))
       (temp2 nil)
       (pos (search ";" str)))
      ((not pos) (if (= i 0) (list temp) (append temp2 (list temp))))
    (progn (setf temp2 (append temp2 (list (subseq temp 0 pos))))
           (setf temp (subseq temp (1+ pos)))
           (setf pos (search ";" temp)))))

(defun take-30-from (list pos)
  (let ((j (* 30 pos)))
    (do ((i j (1+ i))
         (temp2 nil (if (nth i list)
                        (cons (nth i list)
                              temp2)
                      (cons 0 temp2))))
        ((= i (+ j 30))
         (reverse temp2)))))

(defun is-problematic (cmd)
  (do ((problematic-lemmas '("exists T; auto."
                             "assert (Mem.perm m1 b i Max Nonempty)."
                             "assert (UNCHANGED:" "destruct idg as"
                             "eapply T.lub_right; eauto."
                             "eapply T.glb_right; eauto."
                             "+ subst e'; simpl in *."
                             "eapply T.glb_left; eauto. ")
                           (cdr problematic-lemmas))
       (is nil))
      ((or (endp problematic-lemmas)
           is)
       is)
    (if (search (car problematic-lemmas) cmd)
        (setf is t))))

(defun is-in-search (cmd)
  (contains-any cmd useless-terms))

(defun obtain-tactic-result (tactic)
  "Computes the result of the tactic"
  (do ((temp  (cdr tactic)
              (cdr temp))
       (temp2 (if (endp tactic)
                  (list 0 0 0 0 0)
                  (list (first-digit (nth 0 (car tactic))
                                     (digits (nth 0 (car tactic))))
                        (* (rest-of-digits (nth 0 (car tactic))
                                           (digits (nth 0 (car tactic))))
                           (expt 10 (length (cdr tactic))))
                        (* (nth 1 (car tactic))
                           (expt 10 (length (cdr tactic))))
                        (nth 2 (car tactic))
                        (nth 3 (car tactic))))))
      ((endp temp)
       temp2)
    (setf temp2 (list (nth 0 temp2)
                      (+ (nth 1 temp2)
                         (* (expt 10 (length (cdr temp)))
                            (nth 0 (car temp))))
                      (+ (nth 2 temp2)
                         (* (expt 10 (length (cdr temp)))
                            (nth 1 (car temp))))
                      (concat (format "%s" (nth 3 temp2))
                              (format "%s" (nth 2 (car temp))))
                      (+ (nth 4 temp2)
                         (nth 3 (car temp)))))))

(defun first-digit (n digits)
  (/ n (expt 10 (1- digits))))

(defun digits (n)
  (if (= (mod n 10) 0)
      0
    (1+ (digits (/ n 10)))))

(defun rest-of-digits (n digits)
  (- n (* (first-digit n digits) (expt 10 (1- digits)))))

(defun obtain-level-aux1 (level n)
  (* (nth n (car level)) (expt 10 (length (cdr level)))))

(defun obtain-level-aux2 (result n)
  (remove-zeros (nth n result)))

(defun obtain-level-aux3 (result temp n)
  (+ (nth n result) (* (expt 10 (length (cdr temp))) (nth n (car temp)))))

(defun obtain-level (level n)
  (do ((temp (cdr level) (cdr temp))
       (result (if (endp level)
                  (list 0 0 0 0 0 0 0 0 0)
                  (list (obtain-level-aux1 level 0)
                        (obtain-level-aux1 level 1)
                        (obtain-level-aux1 level 2)
                        (obtain-level-aux1 level 3)
                        (obtain-level-aux1 level 4)
                        (obtain-level-aux1 level 5)
                        (obtain-level-aux1 level 6)
                        (obtain-level-aux1 level 7)
                        (nth 8 (car level))))))
      ((endp temp)
       (list (obtain-level-aux2 result 0)
             (obtain-level-aux2 result 1)
             (obtain-level-aux2 result 2)
             (obtain-level-aux2 result 3)
             (obtain-level-aux2 result 4)
             (nth 5 result)
             (obtain-level-aux2 result 6)
             (if (= (nth 7 result) 0)
                 (nth 7 result)
                 (+ (* n (expt 10 (length level)))
                    (obtain-level-aux2 result 7)))
             (nth 8 result)))
    (setf result (list (obtain-level-aux3 result temp 0)
                       (obtain-level-aux3 result temp 1)
                       (obtain-level-aux3 result temp 2)
                       (obtain-level-aux3 result temp 3)
                       (obtain-level-aux3 result temp 4)
                       (obtain-level-aux3 result temp 5)
                       (obtain-level-aux3 result temp 6)
                       (obtain-level-aux3 result temp 7)
                       (+ (nth 8 result) (nth 8 (car temp)))))))

(defun remove-zeros (n)
  "Computes the result of the proof tree level"
  (do ((temp n (/ temp 10)))
      ((or (= temp 0) (not (= (mod temp 10) 0))) temp)))

(defun flat (ll)
  (do ((temp ll (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (setf temp2 (append (car temp) temp2))))

(defun count-seq (item seq)
  (let ((is? (search item seq)))
    (if is?
        (+ 1 (count-seq item (subseq seq (+ 1 is?))))
      0)))
