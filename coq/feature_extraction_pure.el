;; Pure functions taken from feature_extractionv2.el

(defun rem-jumps (cmd)
  (remove-jumps (between-spaces cmd)))

(defun goal-str-aux (s)
  (if s (str-after s "============================\n   ")))

(defun get-type-id-aux (txt)
  (flet ((txt-pos (begin end)
                  (search begin txt :start2 (+ 2 (search end txt)))))
    (subseq txt (+ 2 (search ": " txt))
            (or (txt-pos " " ": ")
                (txt-pos nl  " ")))))

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
  (do ((useless-terms '("Structure" "Section" "Add Ring" "Hypothesis"
                        "Hypotheses" "Include" "Export" "Parameter" "Axiom"
                        "End" "Notation" "Hint" "Inductive" "Variable"
                        "Implicit" "Import" "Canonical" "Coercion" "Next"
                        "Local" "Set" "Instance" "Module" "Ltac" "Let" "Opaque"
                        "Bind" "Scope" "Require" "Infix" "Record" "Fact" "Print"
                        "Arguments" "Function")
                      (cdr useless-terms))
       (is nil))
      ((or (endp useless-terms)
           is)
       is)
    (if (search (car useless-terms) cmd)
        (setf is t))))

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

(defun obtain-level (level n)
  (do ((temp (cdr level)
             (cdr temp))
       (temp2 (if (endp level)
                  (list 0 0 0 0 0 0 0 0 0)
                (list (* (nth 0 (car level))
                         (expt 10 (length (cdr level))))
                      (* (nth 1 (car level))
                         (expt 10 (length (cdr level))))
                      (* (nth 2 (car level))
                         (expt 10 (length (cdr level))))
                      (* (nth 3 (car level))
                         (expt 10 (length (cdr level))))
                      (* (nth 4 (car level))
                         (expt 10 (length (cdr level))))
                      (* (nth 5 (car level))
                         (expt 10 (length (cdr level))))
                      (* (nth 6 (car level))
                         (expt 10 (length (cdr level))))
                      (* (nth 7 (car level))
                         (expt 10 (length (cdr level))))
                      (nth 8 (car level))))))
      ((endp temp)
       (list (remove-zeros (nth 0 temp2))
             (remove-zeros (nth 1 temp2))
             (remove-zeros (nth 2 temp2))
             (remove-zeros (nth 3 temp2))
             (remove-zeros (nth 4 temp2))
             (nth 5 temp2)
             (remove-zeros (nth 6 temp2))
             (if (= (nth 7 temp2)
                    0)
                 (nth 7 temp2)
               (+ (* n (expt 10 (length level)))
                  (remove-zeros (nth 7 temp2))))
             (nth 8 temp2)))
    (setf temp2 (list (+ (nth 0 temp2)
                         (* (expt 10 (length (cdr temp)))
                            (nth 0 (car temp))))
                      (+ (nth 1 temp2)
                         (* (expt 10 (length (cdr temp)))
                            (nth 1 (car temp))))
                      (+ (nth 2 temp2)
                         (* (expt 10 (length (cdr temp)))
                            (nth 2 (car temp))))
                      (+ (nth 3 temp2)
                         (* (expt 10 (length (cdr temp)))
                            (nth 3 (car temp))))
                      (+ (nth 4 temp2)
                         (* (expt 10 (length (cdr temp)))
                            (nth 4 (car temp))))
                      (+ (nth 5 temp2)
                         (* (expt 10 (length (cdr temp)))
                            (nth 5 (car temp))))
                      (+ (nth 6 temp2)
                         (* (expt 10 (length (cdr temp)))
                            (nth 6 (car temp))))
                      (+ (nth 7 temp2)
                         (* (expt 10 (length (cdr temp)))
                            (nth 7 (car temp))))
                      (+ (nth 8 temp2)
                         (nth 8 (car temp)))))))

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
