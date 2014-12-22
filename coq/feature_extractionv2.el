;; Variables to store the tree depth levels

(defvar tdl1 nil)
(defvar tdl2 nil)
(defvar tdl3 nil)
(defvar tdl4 nil)
(defvar tdl5 nil)

;; Variables to store the information about the tactic level

(defvar   intro            nil)
(defvar   case             nil)
(defvar   simpltrivial     nil)
(defvar   induction        nil)
(defvar   simpl            nil)
(defvar   rewrite          nil)
(defvar   trivial          nil)
(defvar   hypothesis       nil)
(defvar   init             0)
(defvar   saved-theorems   nil)
(defvar   goal-level-temp  nil)
(defvar   tactic-level     nil)
(defvar   proof-tree-level nil)
(defvar   theorems_id      nil)
(defvar   add_to           0.1)
(defvar   start            100)
(defconst nl               "
")

;; The integer values associated with the tactics, types and rewrite rules

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

;; Impure functions and macros for building up results

(defun append-hyp (x)
  (setq hypothesis (append hypothesis x)))

(defmacro append-to (name val)
  `(setf ,name (append ,name (list ,val))))

(defun append-to-goal (x)
  (setq goal-level-temp (cons x goal-level-temp)))

(defun append-to-tactic (tactic)
  (unless (assoc tactic tactic_id)
    (append-to tactic_id (cons tactic (1+ (length tactic_id)))))
  (assoc tactic tactic_id))

(defun append-to-theorems (val)
  (append-to theorems_id val))

(defun append-tree (a b c d e f g h i)
  (add-info-to-tree (list a b c d e f g h i) current-level))

(defmacro cons-prepend (name val)
  `(setf ,name (cons ,val ,name)))

(defmacro concat-to (name lst)
  `(setf ,name (concat ,name ,lst)))

(defun append-to-goal-chain (val)
  (append-to-goal val)
  val)

;; Pure functions for text manipulation

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


;; Sending commands to Coq

(defun do-unset-printing ()
  (proof-shell-invisible-cmd-get-result (format "Unset Printing All")))

(defun do-check-object (object)
  (proof-shell-invisible-cmd-get-result (concat "Check " object)))

(defun do-set-printing ()
  (proof-shell-invisible-cmd-get-result (format "Set Printing All")))

(defun do-focus ()
  (proof-shell-invisible-cmd-get-result "Focus"))

(defun do-show-intro ()
  (proof-shell-invisible-cmd-get-result "Show Intro"))

(defun do-intro ()
  (proof-shell-invisible-cmd-get-result "intro"))

(defun do-intro-of (name)
  (proof-shell-invisible-cmd-get-result (concat "intro " name)))

(defun do-show-intros ()
  (proof-shell-invisible-cmd-get-result (format "Show Intros")))

(defun do-undo ()
  (proof-shell-invisible-cmd-get-result (format "Undo")))

(defun do-induction-on (name)
  (proof-shell-invisible-cmd-get-result (concat "induction " name)))

(defun do-show-proof ()
  (proof-shell-invisible-cmd-get-result "Show Proof"))

;; Extracting features

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
         (do-unset-printing)))

(defun get-type-id (object)
  "A function to obtain the type associated with an object"
  (lookup-type-id types_id (get-type-id-aux (do-check-object object))))

(defun lookup-type-id (types id)
  (cdr (or (assoc id types)
           (cons nil -4))))

(defun goal-str ()
  (do-set-printing)
  (goal-str-aux (do-focus)))

(defun get-top-symbol ()
  "Obtain the value of a top symbol"
  (get-top-symbol-aux (goal-str)))

(defun get-obj-intro-aux (obj)
  (subseq obj 0 (search nl obj)))

(defun get-obj-intro ()
  "In some cases the intro tactic does not have parameters. This obtains the
   type of the object introduced with the intro tactic in those cases"
  (proof-undo-last-successful-command)
  (let* ((object (get-obj-intro-aux (do-show-intro))))
    (proof-assert-next-command-interactive)
    (append-hyp (list object))
    (get-type-id object)))

(defun get-types-list (list res)
  "Given a list of objects, obtain the value associated with their types"
  (get-types-list-aux 'get-type-id list res))

(defun get-top-symbols-list (len res)
  "Obtain the value associated with top symbol in the case of intros"
  (get-top-symbols-list-aux 'get-top-symbol 'do-intro len res))

(defun get-top-symbols-seq (seq res)
  (get-top-symbol-seq-aux 'get-top-symbol 'do-intro-of seq res))

(defun get-obj-intros ()
  "Obtain the values associated with intros both for the case when parameters
   are given and the case intros."
  (let* ((undo   (proof-undo-last-successful-command))
         (obj    (do-show-intros)))
    (proof-assert-next-command-interactive)
    (let ((params (extract-params obj nil)))
      (append-hyp params)
      (do-undo)
      (list (get-number-list params)
            (get-types-list params 0)
            (length params)
            (get-top-symbols-list (length params) 0)))))

(defun get-obj-intros2 (objects)
  (let* ((params (extract-params2 objects nil)))
    (append-hyp params)
    (do-undo)
    (list (get-number-list params)
          (get-types-list params 0)
          (length params)
          (get-top-symbols-seq params 0))))

(defun extract-theorem-id (cmd)
  "Look up a theorem's ID. Results are cached, hypotheses have ID 1."
  (let ((s<- (or (search "<-" cmd) 0)))
    (extract-theorem-id-aux (pos-to-dot cmd (if s<- (+ 3 s<-)
                                                    (after-space cmd))))))

(defun extract-theorem-id-aux (arg)
  (cond
   ((assoc arg theorems_id)
    (cdr (assoc arg theorems_id)))

   ((search-in-hyp arg hypothesis)
    1)

   (t
    (setf start (+ start add_to))
    (append-to theorems_id (cons arg start))
    (save-lemma arg start)
    (setf add_to (/ add_to 2))
    start)))

(defun arg-induction (object)
  (do-undo)
  (let ((res (do-check-object object)))
    (do-induction-on object)
    (arg-induction-aux res)))

(defun get-type-id-induction (object arg-ind)
  (let ((check (equal arg-ind 1))
        gt)
    (do-undo)
    (unless check (do-intro-of object))
    (setf gt (get-type-id object))
    (unless check (do-undo))
    (do-induction-on object)
    gt))

(defun add-info-to-tree (info level)
  "Add the information to the corresponding tree depth level"
  (let ((tdl (case level
               (1 'tdl1)
               (2 'tdl2)
               (3 'tdl3)
               (4 'tdl4)
               (5 'tdl5))))
    (when tdl (set tdl (append (symbol-value tdl) (list info))))))

(defun add-info-to-tactic (info tactic)
  "Add the information to the corresponding tactic"
  (let ((tac (case tactic
               ("intro"        'intro)
               ("case"         'case)
               ("simpltrivial" 'simpltrivial)
               ("induction"    'induction)
               ("simpl"        'simpl)
               ("rewrite"      'rewrite)
               ("trivial"      'trivial))))
    (when tac (set tac (append (symbol-value tac) (list info))))))

(defun gn-aux (tree-args tac-info tac hyp thm goal-args ts ngs)
  "Perform the common operations of get-numbers. If hyp or thm are nil, no
   hypothesis/theorem will be appended. To make this clearer, you can create
   your nil values using (not 'some-arbitrary-name), eg.
   (not 'adding-hypothesis) or (not 'adding-theorem)"
  (apply 'append-tree tree-args)
  (add-info-to-tactic tac-info (remove-nonalpha tac))
  (when hyp       (append-hyp hyp))
  (when thm       (append-to-theorems thm))
  (when goal-args (append-to-goal-chain (cons (get-tactic-id tac)
                                              (append goal-args (list ts ngs))))))

(defun get-numbers (cmd tactic ngs ts current-level bot)
  "The first value is the tactic, the second one is the number of tactics,
   the third one is the argument type, the fourth one is if the
   argument is a hypothesis of a theorem, the fifth one is the top-symbol
   and the last one the number of subgoals"
  (cond ((search "- inv H" cmd)
           (list (get-tactic-id "inv") 1 1 -1 ngs ngs))

        ((or (string= cmd "2: eauto.")
             (string= cmd "3: eauto."))
           (let ((res (list (cdr (append-to-tactic "eauto")) 0 0 0 ts ngs)))
             (append-to-goal res)
             (export-tactics)
             res))

        ((string= tactic "intro")
           (let* ((cmd-intro (string= cmd "intro."))
                  (object    (unless cmd-intro
                               (subseq cmd (after-space cmd)
                                           (first-dot cmd))))
                  (type      (if cmd-intro (get-obj-intro)
                                           (get-type-id object))))
             (gn-aux (list type 0 0 0 0 0 0 1 0)
                     (list type -1 ts 1)
                     tactic
                     (unless cmd-intro (list object))
                     nil
                     (list 1 type -1)
                     ts
                     ngs)))

        ((or (string= tactic "intros")
             (string= (subseq cmd 0 (min 8  (length cmd))) "intros [")
             (string= (subseq cmd 0 (min 7  (length cmd))) "intros;")
             (string= (subseq cmd 0 (min 12 (length cmd))) "intros until")
             (search ";intros" cmd))
           (list (get-tactic-id "intro") 1 1 -1 ngs ngs))

        ((string= tactic "case")
           (let* ((object (subseq cmd (after-space cmd) (first-dot cmd)))
                  (type   (get-type-id object)))
             (gn-aux (list 0 type 0 0 0 0 0 2 0)
                     (list type 1 ts 1)
                     tactic
                     nil
                     nil
                     (list 1 type 1)
                     ts ngs)))

        ((or (string= tactic "simpl")
             (string= tactic "trivial"))
           (gn-aux (if (string= tactic "simpl")
                       (list 0 0 0 0 ts 0 0 1 0)
                       (list 0 0 0 0 0 0 ts 1 1))
                   (list 0 0 ts 1)
                   tactic
                   nil
                   nil
                   (list 1 0 0)
                   ts
                   ngs))

        ((search "induction 1" cmd)
           (list (get-tactic-id "induction") 1 1 1 ts ngs))

        ((string= tactic "induction")
           (let* ((object  (subseq cmd (after-space cmd) (first-dot cmd)))
                  (arg-ind (arg-induction object))
                  (type    (get-type-id-induction object arg-ind)))
             (gn-aux (list 0 0 0 type 0 0 0 2 0)
                     (list type arg-ind ts 1)
                     tactic
                     nil
                     (cons (concat "IH" object) 10)
                     (list 1 type arg-ind)
                     ts ngs)))

        ((string= tactic "rewrite")
           (let ((xid (extract-theorem-id cmd)))
             (gn-aux (list 0 0 0 0 0 xid 0 1 0)
                     (list -4 xid ts 1)
                     tactic
                     nil
                     nil
                     (list 1 -4 xid)
                     ts ngs)))

        ((string= cmd "simpl; trivial.")
           (gn-aux (list 0 0 ts 0 0 0 0 1 1)
                   (list 0 0 ts 1)
                   cmd
                   nil
                   nil
                   (list 2 0 0)
                   ts ngs))

        ((string= tactic "red.")
           (append-to-goal-chain (list (get-tactic-id tactic) 0 0 0 ts ngs)))

        (t
           (append-to-goal-chain (list (cdr (append-to-tactic tactic)) 0 0 0 ts ngs)))))

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

(defun get-tactic-id (tac)
  (cdr (assoc (remove-dots tac) tactic_id)))

(defun get-numbers2 (cmd tactic ngs ts current-level bot)
  "Function to obtain the information just about the goals."
  (let ((tacid (get-tactic-id tactic)))
    (cond
     ((string= tactic "intro")
      (let* ((cmd-intro (string= cmd "intro."))
             (object    (unless cmd-intro (subseq cmd (after-space cmd)
                                                      (first-dot   cmd))))
             (type      (if cmd-intro (get-obj-intro)
                                      (get-type-id object))))
        (gn-aux (list type 0 0 0 0 0 0 1 0)
                (list type -1 ts 1)
                tactic
                (unless cmd-intro (list object))
                nil
                (list 1 type -1)
                ts ngs)))

     ((string= tactic "intros")
      (let* ((cmd-intros   ))
        (match (if (string= cmd "intros.")
                   (get-obj-intros)
                   (get-obj-intros2 (subseq cmd (after-space cmd))))
          ((list nparams types-params len gts)
             (let ((arg1         (list types-params 0 0 0 0 0 0 1 0))
                   (arg2         (list types-params -1 gts len)))
               (gn-aux arg1 arg2 "intro" nil nil nil ts ngs)
               (append-to-goal-chain (list nparams len types-params -1 gts ngs)))))))

     ((string= tactic "case")
      (let* ((object (subseq cmd (after-space cmd) (first-dot cmd)))
             (type   (get-type-id object)))
        (append-tree 0 type 0 0 0 0 0 2 0)
        (add-info-to-tactic (list type 1 ts 1) tactic)
        (append-to-goal-chain (list tacid 1 type 1 ts ngs))))

     ((or (string= tactic "simpl")
          (string= tactic "trivial"))
      (let ((tac-simpl (string= tactic "simpl"))
            (bit1      (if tac-simpl ts 0))
            (bit2      (if tac-simpl 0  ts)))
        (append-tree 0 0 0 0 bit1 0 bit2 1 0)
        (add-info-to-tactic (list 0 0 ts 1) tactic)
        (list tacid 1 0 0 ts ngs)))

     ((string= "induction 1" (subseq cmd 0 (min 11 (length cmd))))
      (list (get-tactic-id "induction") 1 1 1 ts ngs))

     ((string= tactic "induction")
      (let* ((object  (subseq cmd (after-space cmd) (first-dot cmd)))
             (arg-ind (arg-induction object))
             (type    (get-type-id-induction object arg-ind)))
        (append-tree 0 0 0 type 0 0 0 2 0)
        (add-info-to-tactic (list type arg-ind ts 1) tactic)
        (setf theorems_id (append theorems_id (list (cons (concat "IH" object) 10))))
        (append-to-goal-chain (list tacid 1 type arg-ind ts ngs))))

     ((or (string= tactic "rewrite")
          (string= cmd "simpl; trivial."))
      (let ((tac-rewrite (string= tactic "rewrite"))
            (bit1        (if tac-rewrite  0 ts))
            (bit2        (if tac-rewrite (extract-theorem-id cmd)
                                          0))
            (bit3        (if tac-rewrite  0 1))
            (bit4        (if tac-rewrite -4 0))
            (bit6        (if tac-rewrite  tacid
                                          (get-tactic-id cmd)))
            (bit7        (if tac-rewrite  1 2)
                         ))
        (append-tree 0 0 bit1 0 0 bit2 0 1 bit3)
        (add-info-to-tactic (list bit4 bit2 ts 1)
                            (if tac-rewrite tactic (remove-nonalpha cmd)))
        (list bit6 bit7 bit4 bit2 ts ngs))))))

(defun count-seq (item seq)
  (let ((is? (search item seq)))
    (if is?
    (+ 1 (count-seq item (subseq seq (+ 1 is?))))
    0)))

(defun get-number-of-goals ()
  (let ((r (do-show-proof)))
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

(defvar problematic-lemmas )

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

(defun split-feature-vector (name fv)
  (let ((len (1+ (floor (length fv) 30))))
    (do ((i 0 (1+ i)))
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
             (export-theorem-aux result (between-spaces cmd) current-level dot-level i)
             (proof-assert-next-command-interactive))

          ((search "Lemma" cmd)
             (proof-assert-next-command-interactive)
             (export-theorem-aux result (rem-jumps cmd) current-level dot-level i))

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

          (t
             (setf ts (get-top-symbol))
             (setf ng (get-number-of-goals))
             (proof-assert-next-command-interactive)
             (setf ng2 (get-number-of-goals))
             (let ((arg (look-through-commands cmd result ts current-level)))
              (cond
               ((< ng ng2)
                  (export-theorem-aux arg name (1+ current-level) (1+ current-level) (1+ i)))

               ((< ng2 ng)
                  (export-theorem-aux arg name current-level      dot-level          (1+ i)))

               (t
                  (export-theorem-aux arg name (1+ current-level) dot-level          (1+ i)))))))))

(defun look-through-commands (cmd start-result ts current-level)
  (do ((cmds       (list-of-commands cmd)
                   (cdr cmds))
       (end-result start-result))
      ((endp cmds)
       end-result)
    (cons-prepend end-result (get-numbers cmd (subseq (car cmds)
                                                      0
                                                      (or (first-space (car cmds))
                                                          (length      (car cmds))))
                                          (get-number-of-goals)
                                          ts current-level 1))))

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
  (do ((theorems saved-theorems (cdr theorems))
       (names    "")
       (i        1              (1+ i)))
      ((endp theorems) names)
    (concat-to names (format "%s %s\n" i (remove_last_colon (caar theorems))))))

(defun extract-names2 (nam)
  (do ((theorems saved-theorems (cdr theorems))
       (names    "")
       (i        1              (1+ i)))
      ((endp theorems)
       names)
    (let ((rjct (remove-jumps (caar theorems))))
      (if (not (string= rjct ""))
          (concat-to names (format "%s %s:%s\n" i nam (remove-last-col rjct)))))))

(defun print-list (list)
  (do ((lst list (cdr lst))
       (str ""))
      ((endp lst) (subseq str 0 (1- (length str))))
    (concat-to str (format "%s," (car lst)))))

(defun extract-features-1 ()
  (let ((fm (longest-theorem)))
    (do ((theorems saved-theorems (cdr theorems))
         (features ""))
        ((endp theorems)
         features)
      (let ((theorem (cadar theorems)))
        (concat-to features (format "%s\n" (print-list (take-30 (if (< (length theorem) fm)
                                                                    (append theorem (generate-zeros (- fm (length theorem))))
                                                                  theorem)))))))))

(defun extract-features-2 (list)
  (do ((feature-list list (cdr feature-list))
       (features     ""))
      ((endp feature-list) features)
    (concat-to features (format "%s\n" (print-list (car feature-list))))))

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
    (while (< (point) final)
      (let* ((semis     (save-excursion
                          (skip-chars-backward " \t\n"
                                               (proof-queue-or-locked-end))
                          (proof-segment-up-to-using-cache (point))))
             (comment   (caar semis))
             (cmd       (cadar semis))
             (pos_dot   (first-dot cmd))
             (pos_space (first-space cmd))
             (ts        nil))
        (setf ts (get-top-symbol))
        (setf ng (get-number-of-goals))
        (proof-assert-next-command-interactive)
        (setf ng2 (get-number-of-goals))
        (setf result (cons (get-numbers2 cmd (subseq cmd 0 (or pos_space pos_dot))
                                         (get-number-of-goals)
                                         ts current-level (if (< ng ng2) 1 0))
                           result))

        (setf current-level (1+ current-level))))

    (take-30 (append (flat (reverse result))
                     (generate-zeros 20)))))

(defun extract-features-1-bis (thm)
  (let ((fm (longest-theorem)))
    (do ((all-theorems saved-theorems (cdr all-theorems))
         (str-result   ""))
        ((endp all-theorems)
         (concat str-result (format "%s\n" (print-list thm))))

      (let* ((theorem (cadar all-theorems))
             (len     (length theorem)))
        (setf str-result (concat str-result
                                 (format "%s\n"
                                         (print-list (take-30 (if (< len fm)
                                                                  theorem
                                                                (append theorem
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
  (do ((all-theorems  saved-theorems (cdr all-theorems))
       (keep-theorems nil))
      ((endp all-theorems)
       keep-theorems)
    (let ((theorem (car all-theorems)))
      (unless (or (equal   (car theorem) nil)
                  (string= (car theorem) ""))
        (setf keep-theorems (append (list theorem) keep-theorems))))))
