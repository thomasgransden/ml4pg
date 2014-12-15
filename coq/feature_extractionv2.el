;; Variables to store the tree depth levels

(defvar tdl1 nil)
(defvar tdl2 nil)
(defvar tdl3 nil)
(defvar tdl4 nil)
(defvar tdl5 nil)

;; Variables to store the information about the tactic level

(defvar intro nil)
(defvar case nil)
(defvar simpltrivial nil)
(defvar induction nil)
(defvar simpl nil)
(defvar rewrite nil)
(defvar trivial nil)

(defvar hypothesis nil)

(defvar init 0)

(defun append-hyp (x)
  (setf hypothesis (append hypothesis x)))

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

(defun export-theorem ()
  (interactive)
  (progn (setf tdl1         nil
               tdl2         nil
               tdl3         nil
               tdl4         nil
               tdl5         nil
               intro        nil
               case         nil
               simpltrivial nil
               induction    nil
               simpl        nil
               rewrite      nil
               trivial      nil
               hypothesis   nil
               goal-level   nil)
         (init-lemmas)
         (export-theorem-aux nil "" 1 1 0)
         (proof-shell-invisible-cmd-get-result (format "Unset Printing All"))))

(defvar saved-theorems   nil)
(defvar goal-level-temp  nil)
(defvar tactic-level     nil)
(defvar proof-tree-level nil)

;; Variables to store the different values associated with the tactics, the
;; types or the rewrite rules

(defvar tactic_id '(("intro"          . 1)
                    ("case"           . 2)
                    ("simpl"          . 3)
                    ("trivial"        . 4)
                    ("induction"      . 5)
                    ("rewrite"        . 6)
                    ("red"            . 7)
                    ("simpl; trivial" . 34)))

(defvar types_id '(("nat"  . -2)
                   ("Prop" . -4)
                   ("bool" . -3)
                   ("A"    . -1)
                   ("list" . -5)))

(defvar theorems_id nil)

(defconst nl "
")

(defun get-type-id (object)
  "A function to obtain the type associated with an object"
  (let* ((a         (proof-shell-invisible-cmd-get-result (format (concat "Check " object))))
         (pos_jump  (search nl  a :start2 (+ 2 (search " "  a))))
         (pos_space (search " " a :start2 (+ 2 (search ": " a))))
         (type (cdr (assoc (subseq a (+ 2 (search ": " a))
                                   (if pos_space pos_space
                                                 pos_jump)
                                   types_id)))))
    (if type type -4)))

;; A function to obtain the value of a top symbol

(defun get-top-symbol ()
  (proof-shell-invisible-cmd-get-result (format "Set Printing All"))
  (let* ((res        (proof-shell-invisible-cmd-get-result (format "Focus")))
         (res2       (subseq res (+ 32 (search "============================" res))))
         (fst-symbol (subseq res2 0 (search " " res2))))
    (cond ((string= fst-symbol "forall") 5)
          ((search "->" res2)            7)
          ((string= "@eq" fst-symbol)    6)
          ((string= "and" fst-symbol)    4)
          ((string= "iff" fst-symbol)    8)
          ((string= "or" fst-symbol)     3)
          (t                             0))))

(defun get-obj-intro ()
  "In some cases the intro tactic does not have parameters. This obtains the
   type of the object introduced with the intro tactic in those cases"
  (let* ((undo   (proof-undo-last-successful-command))
         (obj    (proof-shell-invisible-cmd-get-result (format "Show Intro")))
         (object (subseq obj 0 (search nl obj)))
         (dod    (proof-assert-next-command-interactive))
         (foo    (append-hyp (list object))))
    (get-type-id object)))

(defun extract-params (seq res)
  (let ((pos_space (search " " seq))
        (pos_jump (search nl seq)))
    (if pos_space
        (extract-params (subseq seq (+ 1 pos_space))
                        (cons (subseq seq 0 pos_space)
                              res))
      (reverse (cons (subseq seq 0 pos_jump)
                     res)))))

(defun extract-params2 (seq res)
  (let ((pos_space (search " " seq))
        (pos_jump (search "." seq)))
    (if pos_space
        (extract-params2 (subseq seq (+ 1 pos_space))
                         (cons (subseq seq 0 pos_space)
                               res))
      (reverse (cons (subseq seq 0 pos_jump)
                     res)))))

(defun get-types-list (list res)
  "Given a list of objects, obtain the value associated with their types"
  (if (endp list)
      (* -1 res)
    (get-types-list (cdr list)
                    (+ (* -1 (get-type-id (car list))
                          (expt 10 (- (length list)
                                      1)))
                       res))))

(defun get-number-list (list)
  "Obtain the number of tactics applied"
  (if (endp list)
      0
      (+ (expt 10 (- (length list) 1))
         (get-number-list (cdr list)))))

(defun get-top-symbols-list (len res)
  "Obtain the value associated with top symbol in the case of intros"
  (if (= len 0)
      res
    (let ((gs (get-top-symbol))
          (ps (proof-shell-invisible-cmd-get-result (format "intro"))))
      (+ (get-top-symbols-list (- len 1)
                               (+ (* gs (expt 10 (- len 1)))
                                  res))))))

(defun get-top-symbols-seq (seq res)
  (if (endp seq)
      res
    (let ((gs (get-top-symbol))
          (ps (proof-shell-invisible-cmd-get-result (format (concat "intro " (car seq))))))
      (+ (get-top-symbols-seq (cdr seq)
                              (+ (* gs (expt 10 (- (length seq)
                                                   1)))
                                 res))))))

(defun get-obj-intros ()
  "Obtain the values associated with intros both for the case when parameters
   are given and the case intros."
  (let* ((undo   (proof-undo-last-successful-command))
         (obj    (proof-shell-invisible-cmd-get-result (format "Show Intros")))
         (dod    (proof-assert-next-command-interactive))
         (params (extract-params obj nil))
         (foo    (append-hyp params))
         (types  (get-types-list params 0))
         (num    (get-number-list params))
         (undo2  (proof-shell-invisible-cmd-get-result (format "Undo")))
         (gts    (get-top-symbols-list (length params)
                                       0)))
    (list num types (length params) gts)))

(defun get-obj-intros2 (objects)
  (let* ((params (extract-params2 objects nil))
         (foo    (append-hyp params))
         (types  (get-types-list params 0))
         (num    (get-number-list params))
         (undo2  (proof-shell-invisible-cmd-get-result (format "Undo")))
         (gts    (get-top-symbols-seq params 0)))
    (list num types (length params) gts)))

(defun search-in-hyp (obj hyp)
  "Obtain the value associated with a theorem"
  (if (endp hyp)
      nil
    (if (string= obj (car hyp))
        t
      (search-in-hyp obj (cdr hyp)))))

(defvar add_to 0.1)
(defvar start 100)

(defun extract-theorem-id (cmd)
  (let* ((s<- (search "<-" cmd))
         (dot (pos-to-dot cmd (+ 3 s<-)))
         (s2d (subseq cmd (after-space cmd) (first-dot cmd))))
    (if s<-
        (if (assoc dot theorems_id)
            (cdr (assoc dot theorems_id))
            (if (search-in-hyp dot hypothesis)
                1
                (progn (setf start (+ start add_to))
                       (setf theorems_id
                             (append theorems_id (list (cons dot start))))
                       (save-lemma dot start)
                       (setf add_to (/ add_to 2))
                       start)))

        (if (assoc s2d theorems_id)
            (cdr (assoc s2d theorems_id))
            (if (search-in-hyp s2d hypothesis)
                1
                (progn (setf start (+ start add_to))
                       (save-lemma s2d start)
                       (setf theorems_id
                             (append theorems_id (list (cons s2d start))))
                       (setf add_to (/ add_to 2))
                       start))))))

(defun arg-induction (object)
  (let* ((ps0 (proof-shell-invisible-cmd-get-result (format "Undo")))
         (res (proof-shell-invisible-cmd-get-result (concat "Check " object)))
         (ps3 (proof-shell-invisible-cmd-get-result (concat "induction " object)))
         (err (search "Error" res)))
    (if err -1 1)))

(defun get-type-id-induction (object arg-ind)
  (if (equal arg-ind 1)
      (let ((ps0 (proof-shell-invisible-cmd-get-result (format "Undo")))
            (gt (get-type-id object))
            (ps3 (proof-shell-invisible-cmd-get-result (concat "induction " object))))
        gt)
    (let ((ps0 (proof-shell-invisible-cmd-get-result (format "Undo")))
          (ps (proof-shell-invisible-cmd-get-result (concat "intro " object)))
          (gt (get-type-id object))
          (ps2 (proof-shell-invisible-cmd-get-result (format "Undo")))
          (ps3 (proof-shell-invisible-cmd-get-result (concat "induction " object))))
      gt)))

(defun add-info-to-tree (info level)
  "Add the information to the corresponding tree depth level"
  (cond ((= level 1)
         (setf tdl1 (append tdl1 (list info))))
        ((= level 2)
         (setf tdl2 (append tdl2 (list info))))
        ((= level 3)
         (setf tdl3 (append tdl3 (list info))))
        ((= level 4)
         (setf tdl4 (append tdl4 (list info))))
        ((= level 5)
         (setf tdl5 (append tdl5 (list info))))
        (t nil)))

(defun add-info-to-tactic (info tactic)
  "Function to add the information to the corresponding tactic"
  (cond ((string= tactic "intro")
         (setf intro (append intro (list info))))
        ((string= tactic "case")
         (setf case (append case (list info))))
        ((string= tactic "simpltrivial")
         (setf simpltrivial (append simpltrivial (list info))))
        ((string= tactic "induction")
         (setf induction (append induction (list info))))
        ((string= tactic "simpl")
         (setf simpl (append simpl (list info))))
        ((string= tactic "rewrite")
         (setf rewrite (append rewrite (list info))))
        ((string= tactic "trivial")
         (setf trivial (append trivial (list info))))
        (t nil)))

(defun append-to-goal (x)
  (setf goal-level-temp (cons g goal-level-temp)))

(defun append-to-tactic (tactic)
  (setf tactic_id (append tactic_id (list (cons tactic  (1+ (length tactic_id)))))))

(defun append-tree (a b c d e f g h i)
  (add-info-to-tree (list a b c d e f g h i) current-level))

(defun get-numbers (cmd tactic ngs ts current-level bot)
  "The first value is the tactic, the second one is the number of tactics,
   the third one is the argument type, the fourth one is if the
   argument is a hypothesis of a theorem, the fifth one is the top-symbol
   and the last one the number of subgoals"
  (cond ((search "- inv H" cmd)
           (list (cdr (assoc "inv" tactic_id)) 1 1 -1 ngs ngs))

        ((or (string= cmd "2: eauto.")
             (string= cmd "3: eauto."))
           (unless (assoc "eauto" tactic_id)
             (append-to-tactic "eauto"))
           (append-to-goal (list (cdr (assoc "eauto" tactic_id)) 0 0 0 ts ngs))
           (when ((assoc "eauto" tactic_id)
                  (export-tactics)))
           (list (cdr (assoc "eauto" tactic_id)) 0 0 0 ts ngs))

        ((and (string= tactic "intro")
              (not (string= cmd "intro.")))
           (let* ((object (subseq cmd (after-space cmd) (first-dot cmd)))
                  (type   (get-type-id object))
                  (ai     (append-tree type 0 0 0 0 0 0 1 0))
                  (ait    (add-info-to-tactic (list type -1 ts 1) "intro"))
                  (foo    (append-hyp (list object)))
                  (res    (list (cdr (assoc "intro" tactic_id)) 1 type -1 ts ngs))
                  (foo2   (append-to-goal res)))
             res))

        ((string= tactic "intro")
           (let* ((type (get-obj-intro))
                  (ai (append-tree type 0 0 0 0 0 0 1 0))
                  (ait (add-info-to-tactic (list type -1 ts 1) "intro"))
                  (res (list (cdr (assoc "intro" tactic_id)) 1 (get-obj-intro) -1 ts ngs))
                  (foo2 (append-to-goal res)))
             res))

        ((string= tactic "intros")
           (list (cdr (assoc "intro" tactic_id)) 1 1 -1 ngs ngs))

        ((or (string= (subseq cmd 0 (max 8 (length cmd)))  "intros [")
             (string= (subseq cmd 0 (max 7 (length cmd)))  "intros;")
             (string= (subseq cmd 0 (max 12 (length cmd))) "intros until")
             (search ";intros" cmd))
           (list (cdr (assoc "intro" tactic_id)) 1 1 -1 ngs ngs))

        ((string= tactic "intros")
           (let* ((params       (get-obj-intros))
                  (nparams      (car    params))
                  (types-params (cadr   params))
                  (len          (caddr  params))
                  (gts          (cadddr params))
                  (ai           (append-tree types-params 0 0 0 0 0 0 1 0))
                  (ait          (add-info-to-tactic (list types-params -1 gts len) "intro"))
                  (res          (list nparams len types-params -1 gts ngs))
                  (foo2         (append-to-goal res)))
             res))

        ((string= tactic "case")
           (let* ((object (subseq cmd (after-space cmd) (first-dot cmd)))
                  (type   (get-type-id object))
                  (ai     (append-tree 0 type 0 0 0 0 0 2 0))
                  (ait    (add-info-to-tactic (list type 1 ts 1) "case"))
                  (res    (list (cdr (assoc "case" tactic_id)) 1 type 1 ts ngs))
                  (foo2   (append-to-goal res)))
             res))

        ((string= tactic "simpl")
           (progn (append-tree 0 0 0 0 ts 0 0 1 0)
                  (add-info-to-tactic (list 0 0 ts 1) "simpl")
                  (append-to-goal (list (cdr (assoc "simpl" tactic_id)) 1 0 0 ts ngs))
                  (list (cdr (assoc "simpl" tactic_id)) 1 0 0 ts ngs)))

        ((string= tactic "trivial")
           (append-tree 0 0 0 0 0 0 ts 1 1)
           (add-info-to-tactic (list 0 0 ts 1) "trivial")
           (append-to-goal (list (cdr (assoc "trivial" tactic_id)) 1 0 0 ts ngs))
           (list (cdr (assoc "trivial" tactic_id)) 1 0 0 ts ngs))

        ((search "induction 1" cmd)
           (list (cdr (assoc "induction" tactic_id)) 1 1 1 ts ngs))

        ((string= tactic "induction")
           (let* ((object  (subseq cmd (after-space cmd) (first-dot cmd)))
                  (arg-ind (arg-induction object))
                  (type    (get-type-id-induction object arg-ind))
                  (ai      (append-tree 0 0 0 type 0 0 0 2 0))
                  (ait     (add-info-to-tactic (list type arg-ind ts 1) "induction"))
                  (ih      (setf theorems_id (append theorems_id (list (cons (concat "IH" object) 10)))))
                  (res     (list (cdr (assoc "induction" tactic_id)) 1 type arg-ind ts ngs))
                  (foo2    (append-to-goal res)))
             res))

        ((string= tactic "rewrite")
           (append-tree 0 0 0 0 0 (extract-theorem-id cmd) 0 1 0)
           (add-info-to-tactic (list -4 (extract-theorem-id cmd) ts 1) "rewrite")
           (append-to-goal (list (cdr (assoc "rewrite" tactic_id)) 1 -4 (extract-theorem-id cmd) ts ngs))
           (list (cdr (assoc "rewrite" tactic_id)) 1 -4 (extract-theorem-id cmd) ts ngs))

        ((string= cmd "simpl; trivial.")
           (append-tree 0 0 ts 0 0 0 0 1 1)
           (add-info-to-tactic (list 0 0 ts 1) "simpltrivial")
           (append-to-goal (list (cdr (assoc "simpl; trivial" tactic_id)) 2 0 0 ts ngs))
           (list (cdr (assoc "simpl; trivial" tactic_id)) 2 0 0 ts ngs))

        ((string= tactic "red.")
           (append-to-goal (list (cdr (assoc "red" tactic_id)) 0 0 0 ts ngs))
           (list (cdr (assoc "red" tactic_id)) 0 0 0 ts ngs))

        (t
           (unless (assoc tactic tactic_id)
             (append-to-tactic tactic))
           (append-to-goal (list (cdr (assoc tactic tactic_id)) 0 0 0 ts ngs))
           (unless (assoc tactic tactic_id)
             (export-tactics))
           (list (cdr (assoc tactic tactic_id)) 0 0 0 ts ngs))))

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

(defun replace-colon-rec ()
  (do ((temp tactic_id (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (setf temp2 (append temp2 (list (cons (replace-colon (car (car temp))) (cdr (car temp))))))))

(defun export-tactics ()
  (with-temp-file (concat home-dir "/coq/tactics")
    (insert (format "%s" (replace-colon-rec)))))

(defun load-tactics ()
  (with-temp-buffer
  (insert-file-contents (concat home-dir "/coq/tactics"))
  (setf tactic_id (convert-tactic_id
           (car (read-from-string (format "%s" (read (current-buffer)))))))))

(defun convert-tactic_id (lst)
  (do ((temp lst (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (setf temp2 (append temp2 (list (cons (replace-plus (format "%s" (car (car temp)))) (cdr (car temp))))))))

(defun get-numbers2 (cmd tactic ngs ts current-level bot)
  "Function to obtain the information just about the goals."
  (cond ((and (string= tactic "intro")
              (not (string= cmd "intro.")))
           (let* ((object (subseq cmd (after-space cmd) (first-dot cmd)))
                  (type (get-type-id object))
                  (ai   (append-tree type 0 0 0 0 0 0 1 0))
                  (ait  (add-info-to-tactic (list type -1 ts 1) "intro"))
                  (foo  (append-hyp (list object)))
                  (res  (list (cdr (assoc "intro" tactic_id)) 1 type -1 ts ngs))
                  (foo2 (append-to-goal res)))
             res))

        ((string= tactic "intro")
           (let* ((type (get-obj-intro))
                  (ai   (append-tree type 0 0 0 0 0 0 1 0))
                  (ait  (add-info-to-tactic (list type -1 ts 1) "intro"))
                  (res  (list (cdr (assoc "intro" tactic_id)) 1 (get-obj-intro) -1 ts ngs))
                  (foo2 (append-to-goal res)))
             res))

        ((and (string= tactic "intros")
              (not (string= cmd "intros.")))
           (let* ((params       (get-obj-intros2 (subseq cmd (after-space cmd))))
                  (nparams      (car params))
                  (types-params (cadr params))
                  (len          (caddr params))
                  (gts          (cadddr params))
                  (ai           (append-tree types-params 0 0 0 0 0 0 1 0))
                  (ait          (add-info-to-tactic (list types-params -1 gts len) "intro"))
                  (res          (list nparams len types-params -1 gts ngs))
                  (foo2         (append-to-goal res)))
             res))

        ((string= tactic "intros")
           (let* ((params       (get-obj-intros))
                  (nparams      (car params))
                  (types-params (cadr params))
                  (len          (caddr params))
                  (gts          (cadddr params))
                  (ai           (append-tree types-params 0 0 0 0 0 0 1 0))
                  (ait          (add-info-to-tactic (list types-params -1 gts len) "intro"))
                  (res          (list nparams len types-params -1 gts ngs))
                  (foo2         (append-to-goal res)))
             res))

        ((string= tactic "case")
           (let* ((object (subseq cmd (after-space cmd)
                                  (first-dot cmd)))
                  (type (get-type-id object))
                  (ai   (append-tree 0 type 0 0 0 0 0 2 0))
                  (ait  (add-info-to-tactic (list type 1 ts 1) "case"))
                  (res  (list (cdr (assoc "case" tactic_id)) 1 type 1 ts ngs))
                  (foo2 (append-to-goal res)))
             res))

        ((string= tactic "simpl")
           (append-tree 0 0 0 0 ts 0 0 1 0)
           (add-info-to-tactic (list 0 0 ts 1) "simpl")
           (list (cdr (assoc "simpl" tactic_id)) 1 0 0 ts ngs))

        ((string= tactic "trivial")
           (append-tree 0 0 0 0 0 0 ts 1 1)
           (add-info-to-tactic (list 0 0 ts 1) "trivial")
           (list (cdr (assoc "trivial" tactic_id)) 1 0 0 ts ngs))

        ((string= "induction 1" (subseq cmd 0 (if (< 11 (length cmd))
                                                  11 (length cmd))))
           (list (cdr (assoc "induction" tactic_id)) 1 1 1 ts ngs))

        ((string= tactic "induction")
           (let* ((object  (subseq cmd (after-space cmd) (first-dot cmd)))
                  (arg-ind (arg-induction object))
                  (type    (get-type-id-induction object arg-ind))
                  (ai      (append-tree 0 0 0 type 0 0 0 2 0))
                  (ait     (add-info-to-tactic (list type arg-ind ts 1) "induction"))
                  (ih (setf theorems_id (append theorems_id (list (cons (concat "IH" object) 10)))))
                  (res (list (cdr (assoc "induction" tactic_id)) 1 type arg-ind ts ngs))
                  (foo2 (append-to-goal res)))
           res))

        ((string= tactic "rewrite")
           (append-tree 0 0 0 0 0 (extract-theorem-id cmd) 0 1 0)
           (add-info-to-tactic (list -4 (extract-theorem-id cmd) ts 1) "rewrite")
           (list (cdr (assoc "rewrite" tactic_id)) 1 -4 (extract-theorem-id cmd) ts ngs))

        ((string= cmd "simpl; trivial.")
           (append-tree 0 0 ts 0 0 0 0 1 1)
           (add-info-to-tactic (list 0 0 ts 1) "simpltrivial")
           (list (cdr (assoc "simpl; trivial" tactic_id)) 2 0 0 ts ngs))))

(defun count-seq (item seq)
  (let ((is? (search item seq)))
    (if is?
    (+ 1 (count-seq item (subseq seq (+ 1 is?))))
    0)))

(defun get-number-of-goals ()
  (let ((r (proof-shell-invisible-cmd-get-result (format "Show Proof"))))
    (count-seq "?" r)))

(defun flat (ll)
  (do ((temp ll (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (setf temp2 (append (car temp) temp2))))

(defun remove-zeros (n)
  "The following function computes the result of the proof tree level"
  (do ((temp n (/ temp 10)))
      ((or (= temp 0) (not (= (mod temp 10) 0))) temp)))

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

(defun compute-proof-result ()
  (append (obtain-level tdl1 1)
      (obtain-level tdl2 2)
      (obtain-level tdl3 3)
      (obtain-level tdl4 4)
      (obtain-level tdl5 5)))

(defun digits (n)
  (if (= (mod n 10) 0)
      0
      (1+ (digits (/ n 10)))))

(defun first-digit (n digits)
  (/ n (expt 10 (1- digits))))

(defun rest-of-digits (n digits)
  (- n (* (first-digit n digits) (expt 10 (1- digits)))))

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

(defun compute-tactic-result ()
  (append (obtain-tactic-result intro)
          (obtain-tactic-result case)
          (obtain-tactic-result simpltrivial)
          (obtain-tactic-result induction)
          (obtain-tactic-result simpl)
          (obtain-tactic-result rewrite)
          (obtain-tactic-result trivial)))

(defvar useless-terms '("Structure" "Section" "Add Ring" "Hypothesis"
                        "Hypotheses" "Include" "Export" "Parameter" "Axiom"
                        "End" "Notation" "Hint" "Inductive" "Variable"
                        "Implicit" "Import" "Canonical" "Coercion" "Next"
                        "Local" "Set" "Instance" "Module" "Ltac" "Let" "Opaque"
                        "Bind" "Scope" "Require" "Infix" "Record" "Fact" "Print"
                        "Arguments" "Function"))

(defvar problematic-lemmas '("exists T; auto."
                             "assert (Mem.perm m1 b i Max Nonempty)."
                             "assert (UNCHANGED:" "destruct idg as"
                             "eapply T.lub_right; eauto."
                             "eapply T.glb_right; eauto."
                             "+ subst e'; simpl in *."
                             "eapply T.glb_left; eauto. "))

(defun is-in-search (cmd)
  (do ((temp useless-terms (cdr temp))
       (is nil))
      ((or (endp temp)
           is)
       is)
    (if (search (car temp) cmd)
        (setf is t))))

(defun is-problematic (cmd)
  (do ((temp problematic-lemmas (cdr temp))
       (is nil))
      ((or (endp temp)
           is)
       is)
    (if (search (car temp) cmd)
        (setf is t))))

(defvar var-cl 1)

(defun split-feature-vector (name fv)
  (let ((len (1+ (floor (length fv) 30))))
    (do ((i 0 (+ i 1)))
        ((equal i len)
         nil)
      (setf saved-theorems (append saved-theorems
                                   (list (list name (take-30-from fv i))))))))

(defun take-30-from (list pos)
  (let ((j (* 30 pos)))
    (do ((i j (1+ i))
         (temp2 nil (if (nth i list)
                        (cons (nth i list)
                              temp2)
                      (cons 0 temp2))))
        ((= i (+ j 30))
         (reverse temp2)))))

(defun export-theorem-aux (result name current-level dot-level i)
  (let* ((semis     (save-excursion
                      (skip-chars-backward " \t\n"
                                           (proof-queue-or-locked-end))
                      (proof-segment-up-to-using-cache (point))))
         (comment   (caar  semis))
         (cmd       (cadar semis))
         (pos_dot   (first-dot   cmd))
         (pos_space (first-space cmd))
         (ts        nil))

    (cond ((or (string= comment "comment")
               (is-in-search cmd))
             (proof-assert-next-command-interactive)
             (export-theorem-aux result name current-level dot-level i))

          ((is-problematic cmd)
             (search-forward "Defined")
             (proof-goto-point)
             (proof-assert-next-command-interactive))

          ((or (search "Definition" cmd)
               (search "Fixpoint"   cmd))
             (proof-assert-next-command-interactive)
             (ignore-errors(adddefinition (between-spaces cmd)))
             (export-theorem-aux result
                                 (between-spaces cmd)
                                 current-level dot-level i)
             (proof-assert-next-command-interactive))

          ((search "Lemma" cmd)
             (proof-assert-next-command-interactive)
             (export-theorem-aux result
                                 (rem-jumps cmd)
                                 current-level dot-level i))

          ((search "Proof" cmd)
             (proof-assert-next-command-interactive)
             (export-theorem-aux result name current-level dot-level i))

          ((search "Instance" cmd)
             (proof-assert-next-command-interactive)
             (export-theorem-aux result
                                 (rem-jumps cmd)
                                 current-level dot-level i))

          ((search "Theorem" cmd)
             (proof-assert-next-command-interactive)
             (export-theorem-aux result
                                 (rem-jumps cmd)
                                 current-level dot-level i))

          ((search "Remark" cmd)
             (proof-assert-next-command-interactive)
             (export-theorem-aux result
                                 (rem-jumps cmd)
                                 current-level dot-level i))

          ((search "Corollary" cmd)
             (proof-assert-next-command-interactive)
             (export-theorem-aux result
                                 (rem-jumps cmd)
                                 current-level dot-level i))

          ((or (search "Qed." cmd)
               (search "Defined." cmd))
             (proof-assert-next-command-interactive)
             (setf proof-tree-level (append proof-tree-level (list (compute-proof-result))))
             (setf tactic-level (append tactic-level (list (compute-tactic-result))))
             (if name
                 (split-feature-vector name (flat (reverse result))))
             (ignore-errors (addthm name)))

          (pos_space
             (setf ts (get-top-symbol))
             (setf ng (get-number-of-goals))
             (proof-assert-next-command-interactive)
             (setf ng2 (get-number-of-goals))
             (cond
              ((< ng ng2)
                 (export-theorem-aux
                  (do ((temp (list-of-commands cmd)
                             (cdr temp))
                       (temp2 result))
                      ((endp temp)
                       temp2)
                    (setf temp2 (cons (get-numbers cmd (subseq (car temp)
                                                               0 (if (search " " (car temp))
                                                                     (search " " (car temp))
                                                                   (length (car temp))))
                                                   (get-number-of-goals)
                                                   ts current-level 1)
                                      temp2)))
                  name
                  (1+ current-level)
                  (1+ current-level)
                  (1+ i)))

              ((< ng2 ng)
                 (export-theorem-aux
                  (do ((temp (list-of-commands cmd)
                             (cdr temp))
                       (temp2 result))
                      ((endp temp)
                       temp2)
                    (setf temp2 (cons (get-numbers cmd (subseq (car temp)
                                                               0 (if (search " " (car temp))
                                                                     (search " " (car temp))
                                                                   (length (car temp))))
                                                   (get-number-of-goals)
                                                   ts current-level 1)
                                      temp2)))
                  name
                  current-level
                  dot-level
                  (1+ i)))

              (t
                 (export-theorem-aux
                  (do ((temp (list-of-commands cmd)
                             (cdr temp))
                       (temp2 result))
                      ((endp temp)
                       temp2)
                    (setf temp2 (cons (get-numbers cmd (subseq (car temp)
                                                               0 (if (search " " (car temp))
                                                                     (search " " (car temp))
                                                                   (length (car temp))))
                                                   (get-number-of-goals)
                                                   ts current-level 1)
                                      temp2)))

                  name
                  (1+ current-level)
                  dot-level
                  (1+ i)))))

          (t
             (progn (setf ts (get-top-symbol))
                    (setf ng (get-number-of-goals))
                    (proof-assert-next-command-interactive)

                    (setf ng2 (get-number-of-goals))
                    (cond
                     ((< ng ng2)
                        (export-theorem-aux
                         (do ((temp (list-of-commands cmd)
                                    (cdr temp))
                              (temp2 result))
                             ((endp temp)
                              temp2)
                           (setf temp2 (cons (get-numbers cmd (subseq (car temp)
                                                                      0 (if (search " " (car temp))
                                                                            (search " " (car temp))
                                                                          (length (car temp))))
                                                          (get-number-of-goals)
                                                          ts current-level 1)
                                             temp2)))
                         name
                         (1+ current-level)
                         (1+ current-level)
                         (1+ i)))

                     ((< ng2 ng)
                        (export-theorem-aux
                         (do ((temp (list-of-commands cmd)
                                    (cdr temp))
                              (temp2 result))
                             ((endp temp)
                              temp2)
                           (setf temp2 (cons (get-numbers cmd
                                                          (subseq (car temp)
                                                                  0
                                                                  (cond (search " " (car temp))
                                                                        (t (length (car temp)))))
                                                          (get-number-of-goals)
                                                          ts
                                                          current-level
                                                          1)
                                             temp2)))

                         name
                         current-level
                         dot-level
                         (1+ i)))

                     (t
                        (export-theorem-aux
                         (do ((temp (list-of-commands cmd)
                                    (cdr temp))
                              (temp2 result))
                             ((endp temp)
                              temp2)
                           (setf temp2 (cons (get-numbers cmd (subseq (car temp)
                                                                      0 (cond (search " " (car temp))
                                                                              (t (length (car temp)))))
                                                          (get-number-of-goals)
                                                          ts current-level 1)
                                             temp2)))
                         name
                         (1+ current-level)
                         dot-level
                         (1+ i)))))))))

(defun list-of-commands (str)
  (do ((temp (subseq str 0 (1- (length str))))
       (i 0 (1+ i))
       (temp2 nil)
       (pos (search ";" str)))
      ((not pos) (if (= i 0) (list temp) (append temp2 (list temp))))
    (progn (setf temp2 (append temp2 (list (subseq temp 0 pos))))
       (setf temp (subseq temp (1+ pos)))
       (setf pos (search ";" temp)))))

(defun save-file-conventions1 ()
  "Save the files"
  (interactive)
  (let* ((file (read-file-name "Save in file (don't include the extension): "))
         (func (lambda (name content)
                 (with-temp-file (concat file name) (insert content)))))
    (progn
      (func "_goals.csv"            extract-features-1)
      (func "_proof_tree.csv"       (extract-features-2 proof-tree-level))
      (func "_tactic.csv"           (extract-features-2 tactic-level))
      (func (format "_summary.txt") extract-names))))

(defun remove-last-col (str)
  (if (string= (subseq str (1- (length str))) ":")
      (subseq str 0 (1- (length str)))
    str))

(defun extract-names ()
  (do ((temp saved-theorems (cdr temp))
       (temp2 "")
       (i 1 (1+ i)))
      ((endp temp) temp2)
    (setf temp2 (concat temp2 (format "%s %s\n" i (remove_last_colon (caar temp)))) )))

(defun extract-names2 (nam)
  (do ((temp  saved-theorems (cdr temp))
       (temp2 "")
       (i 1 (1+ i)))
      ((endp temp)
       temp2)
    (if (not (string= (remove-jumps (caar temp))
                      ""))
        (setf temp2 (concat temp2 (format "%s %s:%s\n" i nam (remove-last-col (remove-jumps (caar temp)))))))))

(defun print-list (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) (subseq temp2 0 (1- (length temp2))))
    (setf temp2 (concat temp2 (format "%s," (car temp))) )))

(defun extract-features-1 ()
  (let ((fm (longest-theorem)))
    (do ((temp  saved-theorems (cdr temp))
         (temp2 ""))
        ((endp temp)
         temp2)
      (if (< (length (cadar temp))
             fm)
          (setf temp2 (concat temp2
                              (format "%s\n"
                                      (print-list (take-30 (append (cadar temp)
                                                                   (generate-zeros (- fm (length (cadar temp))))))))))
        (setf temp2 (concat temp2 (format "%s\n" (print-list (take-30 (cadar temp))))))))))

(defun extract-features-2 (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) temp2)
      (setf temp2 (concat temp2 (format "%s\n" (print-list (car temp)))))))

(defun generate-zeros (n)
  (-unfold (lambda (x)
             (case x
               (0         nil)
               (otherwise (cons 0 (1- x)))))
           n))

(defun longest-theorem ()
  (find-max-length saved-theorems))

(defun find-max-length (lst)
  (-reduce-from (lambda (x y)
                  (max x (length y)))
                0
                lst))

(defun take-30 (list)
  (-take 30 list))

(defun extract-info-up-to-here ()
  "Extract the info of a theorem up to a concrete point"
  (interactive)
  (setf tdl1         nil
        tdl2         nil
        tdl3         nil
        tdl4         nil
        tdl5         nil
        intro        nil
        case         nil
        simpltrivial nil
        induction    nil
        simpl        nil
        rewrite      nil
        trivial      nil)
  (let ((final         (point))
        (result        nil)
        (current-level 1))
    (search-backward "Proof.")
    (proof-goto-point)
    (while (< (point)
              final)
      (let* ((semis     (save-excursion
                          (skip-chars-backward " \t\n"
                                               (proof-queue-or-locked-end))
                          (proof-segment-up-to-using-cache (point))))
             (comment   (caar semis))
             (cmd       (cadar semis))
             (pos_dot   (first-dot cmd))
             (pos_space (first-space cmd))
             (ts        nil))
        (cond (pos_space
               (progn (setf ts (get-top-symbol))
                      (setf ng (get-number-of-goals))
                      (proof-assert-next-command-interactive)
                      (setf ng2 (get-number-of-goals))
                      (cond ((< ng ng2)
                             (progn (setf result (cons (get-numbers2 cmd (subseq cmd 0 pos_space)
                                                                     (get-number-of-goals)
                                                                     ts current-level 1)
                                                       result))
                                    (setf current-level (1+ current-level))))
                            ((< ng2 ng)
                             (progn (setf result (cons (get-numbers2 cmd (subseq cmd 0 pos_space)
                                                                     (get-number-of-goals)
                                                                     ts current-level 0)
                                                       result))
                                    (setf current-level (1+ current-level))))
                            (t (progn (setf result (cons (get-numbers2 cmd (subseq cmd 0 pos_space)
                                                                       (get-number-of-goals)
                                                                       ts current-level 0)
                                                         result))
                                      (setf current-level (1+ current-level)))))))
              (t (progn (setf ts (get-top-symbol))
                        (setf ng (get-number-of-goals))
                        (proof-assert-next-command-interactive)
                        (setf ng2 (get-number-of-goals))
                        (cond ((< ng ng2)
                               (progn (setf result (cons (get-numbers2 cmd (subseq cmd 0 pos_dot)
                                                                       (get-number-of-goals)
                                                                       ts current-level 1)
                                                         result))
                                      (setf current-level (1+ current-level))))
                              ((< ng2 ng)
                               (progn (setf result (cons (get-numbers2 cmd (subseq cmd 0 pos_dot)
                                                                       (get-number-of-goals)
                                                                       ts current-level 0)
                                                         result))
                                      (setf current-level (1+ current-level))))
                              (t (progn (setf result(cons (get-numbers2 cmd (subseq cmd 0 pos_dot)
                                                                        (get-number-of-goals)
                                                                        ts current-level 0)
                                                          result)
                                              )
                                        (setf current-level (1+ current-level))))))))))

    (take-30 (append (flat (reverse result))
                     (generate-zeros 20)))))

(defun extract-features-1-bis (thm)
  (let ((fm (longest-theorem)))
    (do ((temp saved-theorems (cdr temp))
         (temp2 ""))
        ((endp temp)
         (concat temp2 (format "%s\n" (print-list thm))))

      (let* ((len (length (cadar temp)))
             (lst ))
        (setf temp2 (concat temp2
                            (format "%s\n"
                                    (print-list (take-30 (if (< len fm)
                                                             (cadar temp)
                                                             (append (cadar temp)
                                                                     (generate-zeros (- fm len)))))))))))))

(defun extract-feature-theorems ()
  "Extract the information from all the theorems up to a point"
  (interactive)
  (let ((final         (point))
        (current-level 1))
    (export-theorem)
    (while (< (point) final)
      (export-theorem)))
  (setf saved-theorems (remove-nil-cases)))

(defun remove-nil-cases ()
  (do ((temp  saved-theorems (cdr temp))
       (temp2 nil))
      ((endp temp)
       temp2)
    (if (or (equal (car (car temp))
                   nil)
            (string= (car (car temp))
                     ""))
        nil
        (setf temp2 (append (list (car temp))
                            temp2)))))
