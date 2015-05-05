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

(defvar useless-terms '("Structure" "Section" "Add Ring" "Hypothesis"
                        "Hypotheses" "Include" "Export" "Parameter" "Axiom"
                        "End" "Notation" "Hint" "Inductive" "Variable"
                        "Implicit" "Import" "Canonical" "Coercion" "Next"
                        "Local" "Set" "Instance" "Module" "Ltac" "Let" "Opaque"
                        "Bind" "Scope" "Require" "Infix" "Record" "Fact" "Print"
                        "Arguments" "Function"))

;; Impure functions and macros for building up results

(defun append-hyp (x)
  (setq hypothesis (append hypothesis x)))

(defun append-to-goal (x)
  (setq goal-level-temp (cons x goal-level-temp)))

(defun append-to-tactic (tactic)
  (unless (assoc tactic tactic_id)
    (append-to tactic_id (cons tactic (1+ (length tactic_id)))))
  (cdr (assoc tactic tactic_id)))

(defun append-to-theorems (val)
  (append-to theorems_id val))

(defun append-tree (a b c d e f g h i)
  (add-info-to-tree (list a b c d e f g h i) current-level))

(defun append-to-goal-chain (val)
  (append-to-goal val)
  val)

;; Extracting features

(defun export-theorem ()
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
        trivial      nil
        hypothesis   nil
        goal-level   nil)
  (init-lemmas)
  (let ((result (export-theorem-aux nil "" 1 1 0)))
    (test-msg (format "ET POINT %s PROOF %s" (point) (proof-queue-or-locked-end)))
    (do-unset-printing)
    (test-msg (format "ET2 POINT %s PROOF %s" (point) (proof-queue-or-locked-end)))
    result))

(defun get-type-id (object)
  "A function to obtain the type associated with an object"
  (let ((check (do-check-object object)))
    (unless check
      (error "No such object %S" object))
    (lookup-type-id types_id (get-type-id-aux check))))

(defun lookup-type-id (types id)
  (cdr (or (assoc id types)
           (cons nil -4))))

(defun get-top-symbol (&optional handler)
  "Obtain the value of a top symbol"
  (get-top-symbol-aux (do-goal-str handler)))

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

(defun get-obj-intros-aux (objects extractor getter alter)
  (let ((params (apply extractor (list objects nil))))
    (append-hyp params)
    (do-undo)
    (list (get-number-list params)
          (get-types-list params 0)
          (length params)
          (apply getter (list (apply alter params) 0)))))

(defun get-obj-intros ()
  "Obtain the values associated with intros both for the case when parameters
   are given and the case intros."
  (let* ((undo    (proof-undo-last-successful-command))
         (objects (do-show-intros)))
    (proof-assert-next-command-interactive)
    (get-obj-intros-aux objects 'extract-params 'get-top-symbols-list 'length)))

(defun get-obj-intros2 (objects)
  (get-obj-intros-aux objects 'extract-params2 'get-top-symbols-seq 'id))

(defun extract-theorem-id (cmd)
  "Look up a theorem's ID. Results are cached, hypotheses have ID 1."
  (extract-theorem-id-aux2 (extract-theorem-id-aux cmd)))

(defun extract-theorem-id-aux2 (arg)
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
  (let ((check (and (equal arg-ind 1)
                    (condition-case nil
                       (progn (get-type-id object)
                              t)
                     (error))))
        gt)
    (do-undo)
    (test-msg (format "CHECK %S" check))
    (unless check (do-intro-of object))
    (setf gt (get-type-id object))
    (unless check (do-undo))
    (do-induction-on object)
    gt))

(defun with-name-introduced (name f)
  (do-intro-of name)
  (let ((result (funcall f)))
    (do-undo)
    result))

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

(defun gn-aux (tree-args tac-info goal-args tac ts ngs &optional hyp thm)
  "Perform the common operations of get-numbers. If hyp or thm are nil, no
   hypothesis/theorem will be appended. To make this clearer, you can create
   your nil values using (not 'some-arbitrary-name), eg.
   (not 'adding-hypothesis) or (not 'adding-theorem)"
  (when tree-args (apply 'append-tree tree-args))
  (when tac-info  (add-info-to-tactic tac-info (remove-nonalpha tac)))
  (when hyp       (append-hyp hyp))
  (when thm       (append-to-theorems thm))
  (when goal-args (append-to-goal-chain (cons (get-tactic-id tac)
                                              (append goal-args (list ts ngs))))))

(defun gn-branch-intro (cmd)
  (let* ((cmd-intro (string= cmd "intro."))
         (object    (unless cmd-intro (get-numbers-get-object cmd)))
         (type      (if cmd-intro (get-obj-intro)
                                  (get-type-id object))))
    (list (list type 0 0 0 0 0 0 1 0)
          (list type -1)
          (list 1 type -1)
          nil
          (unless cmd-intro (list object)))))

(defun gn-branch-case (cmd)
  (let ((type (get-type-id (get-numbers-get-object cmd))))
    (list (list 0 type 0 0 0 0 0 2 0)
          (list type 1)
          (list 1 type 1))))

(defun gn-branch-induction (cmd)
  (test-msg (format "GNBI %s %s" (proof-queue-or-locked-end) (point)))
  (let* ((object  (get-numbers-get-object cmd))
         (arg-ind (arg-induction object))
         (type    (get-type-id-induction object arg-ind)))
    (list (list 0 0 0 type 0 0 0 2 0)
          (list type arg-ind)
          (list 1 type arg-ind)
          nil
          nil
          (cons (concat "IH" object) 10))))

(defun gn-branch-rewrite (cmd)
  (let ((xid (extract-theorem-id cmd)))
    (list (list 0 0 0 0 0 xid 0 1 0)
          (list -4 xid)
          (list 1 -4 xid))))

(defun get-numbers-apply (tactic cmd ts ngs)
  `(lambda (tree-args tac-info goal-args &optional tac hyp thm)
     (gn-aux tree-args
             (append tac-info (list ,ts 1))
             goal-args
             (if tac ,cmd ,tactic)
             ,ts ,ngs hyp thm)))

(defconst no-tacinfo (list 0 0))

(defun get-numbers (cmd tactic ngs ts current-level bot)
  "The first value is the tactic, the second one is the number of tactics,
   the third one is the argument type, the fourth one is if the
   argument is a hypothesis of a theorem, the fifth one is the top-symbol
   and the last one the number of subgoals"
  (test-msg (format "GN %S %S" (point) (proof-queue-or-locked-end)))
  (let ((process (get-numbers-apply tactic cmd ts ngs)))
    (cond
     ((search "- inv H" cmd)
        (list (get-tactic-id "inv") 1 1 -1 ngs ngs))

     ((or (string= cmd "2: eauto.")
          (string= cmd "3: eauto."))
        (let ((result (list (append-to-tactic "eauto") 0 0 0 ts ngs)))
          (append-to-goal result)
          (export-tactics)
          result))

     ((string= tactic "intro")
        (apply process (gn-branch-intro cmd)))

     ((or (string= tactic "intros")
          (is-intro cmd))
        (list (get-tactic-id "intro") 1 1 -1 ngs ngs))

     ((string= tactic "case")
        (apply process (gn-branch-case cmd)))

     ((string= tactic "simpl")
        (apply process (list (list 0 0 0 0 ts 0 0 1 0)
                             no-tacinfo
                             (list 1 0 0))))

     ((string= tactic "trivial")
        (apply process (list (list 0 0 0 0 0 0 ts 1 1)
                             no-tacinfo
                             (list 1 0 0))))

     ((search "induction 1" cmd)
        (list (get-tactic-id "induction") 1 1 1 ts ngs))

     ((string= tactic "induction")
        (apply process (gn-branch-induction cmd)))

     ((string= tactic "rewrite")
        (apply process (gn-branch-rewrite cmd)))

     ((string= cmd "simpl; trivial.")
        (apply process (list (list 0 0 ts 0 0 0 0 1 1)
                             no-tacinfo
                             (list 2 0 0)
                             t)))

     (t
        (append-to-goal-chain (list (if (string= tactic "red.")
                                        (get-tactic-id tactic)
                                        (append-to-tactic tactic))
                                    0 0 0 ts ngs))))))

(defun replace-colon-rec ()
  (do ((temp tactic_id (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (setf temp2 (append temp2 (list (cons (replace-colon (car (car temp))) (cdr (car temp))))))))

(defun export-tactics ()
  (with-temp-file (concat home-dir "coq/tactics")
    (insert (format "%s" (replace-colon-rec)))))

(defun get-tactic-id (tac)
  (cdr (assoc (remove-dots tac) tactic_id)))

(defun gn2-intro (cmd)
  (let* ((cmd-intro (string= cmd "intro."))
         (object    (unless cmd-intro (get-numbers-get-object cmd)))
         (type      (if cmd-intro (get-obj-intro)
                                  (get-type-id object))))
    (list (list type 0 0 0 0 0 0 1 0)
          (list type -1)
          (list 1 type -1)
          nil
          (unless cmd-intro (list object)))))

(defun gn2-intros (cmd ts ngs)
  (let* ((params (if (string= cmd "intros.")
                     (get-obj-intros)
                     (get-obj-intros2 (subseq cmd (after-space cmd)))))
         (nparams      (nth 0 params))
         (types-params (nth 1 params))
         (len          (nth 2 params))
         (gts          (nth 3 params)))
    (gn-aux (list types-params 0 0 0 0 0 0 1 0)
            (list types-params -1 gts len)
            nil
            "intro"
            ts
            ngs)
    (append-to-goal-chain (list nparams len types-params -1 gts ngs))))

(defun gn2-case (cmd)
  (let* ((object (get-numbers-get-object cmd))
         (type   (get-type-id object)))
    (list (list 0 type 0 0 0 0 0 2 0)
          (list type 1)
          (list 1 type 1))))

(defun gn2-simpl (ts ngs tactic tacid)
  (gn-aux (list 0 0 0 0 ts 0 0 1 0)
          (list 0 0 ts 1)
          nil
          tactic
          ts
          ngs)
  (list tacid 1 0 0 ts ngs))

(defun gn2-trivial (ts ngs tactic tacid)
  (append-tree 0 0 0 0 0 0 ts 1 0)
  (add-info-to-tactic (list 0 0 ts 1) tactic)
  (list tacid 1 0 0 ts ngs))

(defun gn2-induction (cmd)
  (let* ((object  (get-numbers-get-object cmd))
         (arg-ind (arg-induction object))
         (type    (get-type-id-induction object arg-ind)))
    (list (list 0 0 0 type 0 0 0 2 0)
          (list type arg-ind)
          (list 1 type arg-ind)
          nil
          nil
          (cons (concat "IH" object) 10))))

(defun gn2-rewrite (cmd tactic tacid ts ngs)
  (let ((tid (extract-theorem-id cmd)))
    (append-tree 0 0 0 0 0 tid 0 1 0)
    (add-info-to-tactic (list -4 tid ts 1) tactic)
    (list tacid 1 -4 tid ts ngs)))

(defun gn2-simpltrivial (cmd ts ngs)
  (append-tree 0 0 ts 0 0 0 0 1 1)
  (add-info-to-tactic (list 0 0 ts 1) (remove-nonalpha cmd))
  (list (get-tactic-id cmd) 2 0 0 ts ngs))

(defun get-numbers2 (cmd tactic ngs ts current-level bot)
  "Obtain the information just about the goals."
  (let ((tacid (get-tactic-id tactic))
        (process (get-numbers-apply tactic cmd ts ngs)))
    (cond
     ((string= tactic "intro")
        (apply process (gn2-intro cmd)))

     ((string= tactic "intros")
        (gn2-intros cmd ts ngs))

     ((string= tactic "case")
        (apply process (gn2-case cmd)))

     ((string= tactic "simpl")
        (gn2-simpl ts ngs tactic tacid))

     ((string= tactic "trivial")
        (gn2-trivial ts ngs tactic tacid))

     ((string= "induction 1" (subseq cmd 0 (min 11 (length cmd))))
        (list (get-tactic-id "induction") 1 1 1 ts ngs))

     ((string= tactic "induction")
        (apply process (gn2-induction cmd)))

     ((string= tactic "rewrite")
        (gn2-rewrite cmd tactic tacid ts ngs))

     ((string= cmd "simpl; trivial.")
        (gn2-simpltrivial cmd ts ngs)))))

(defun compute-proof-tree-result (name)
  (append (obtain-level tdl1 1)
          (obtain-level tdl2 2)
          (obtain-level tdl3 3)
          (obtain-level tdl4 4)
          (obtain-level tdl5 5)))

(defun compute-tactic-result (name)
  (append (obtain-tactic-result intro)
          (obtain-tactic-result case)
          (obtain-tactic-result simpltrivial)
          (obtain-tactic-result induction)
          (obtain-tactic-result simpl)
          (obtain-tactic-result rewrite)
          (obtain-tactic-result trivial)))

(defun split-feature-vector (name fv)
  (let ((len (1+ (floor (length fv) 30))))
    (do ((i 0 (1+ i)))
        ((equal i len)
         nil)
      (setf saved-theorems (append saved-theorems
                                   (list (list name (take-30-from fv i))))))))

(defun export-theorem-aux (result name current-level dot-level i)
  (test-msg "EXPORTING THEOREM AUX")
  (export-theorem-aux2 result name (list current-level dot-level i
                                         ;; Goal count goes up
                                         (lambda (current-level dot-level i up down same)
                                           (list (1+ current-level)
                                                 (1+ current-level)
                                                 (1+ i)
                                                 up down same))
                                         ;; Goal count goes down
                                         (lambda (current-level dot-level i up down same)
                                           (list current-level
                                                 dot-level
                                                 (1+ i)
                                                 up down same))
                                         ;; Goal count stays the same
                                         (lambda (current-level dot-level i up down same)
                                           (list (1+ current-level)
                                                 dot-level
                                                 (1+ i)
                                                 up down same)))))

(defun export-theorem-problematic ()
  (search-forward "Defined")
  (proof-goto-point)
  (proof-assert-next-command-interactive))

(defun export-theorem-otherwise (cmd result name args)
  (test-msg (format "ETO %S %S" (point) (proof-queue-or-locked-end)))
  (add-hypotheses name)
  (let ((try-ts (get-top-symbol (lambda (x) nil))))
    (when try-ts
      (setf ts try-ts)
      (setf ng  (get-number-of-goals))
      (proof-assert-next-command-interactive)
      (setf ng2 (get-number-of-goals))
      (let ((arg (look-through-commands cmd result ts (nth 0 args))))
        (export-theorem-aux2 arg name (apply (nth (cond ((< ng  ng2) 3)
                                                        ((< ng2 ng)  4)
                                                        (t           5)) args)
                                             args))))))

(defun look-through-commands (cmd start-result ts current-level)
  (let ((end-result start-result))
    (dolist (elem (list-of-commands cmd) end-result)
      (cons-prepend end-result
                    (get-numbers cmd (subseq elem 0 (or (first-space elem)
                                                        (length      elem)))
                                 (get-number-of-goals)
                                 ts
                                 current-level
                                 1)))))

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

(defun longest-theorem ()
  (find-max-length saved-theorems))

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
  (let* ((final         (point))
         (result        nil)
         (last          final)
         (try-ts        t)
         (current-level 1))
    ;(when (search-backward "Proof." nil t))
    (proof-goto-point)
    (setf last (1- (point)))
    (while (and (< (point) final)
                (> (point) last)
                try-ts)
      (setf last (point))
      (let* ((semis     (get-semis))
             (comment   (caar semis))
             (cmd       (cadar semis))
             (pos_dot   (first-dot cmd))
             (pos_space (first-space cmd))
             (ts        nil))
        (setf try-ts (get-top-symbol (lambda (&rest x) nil)))
        (when try-ts
          (setf ts try-ts)
          (setf ng (get-number-of-goals))
          (proof-assert-next-command-interactive)
          (setf ng2 (get-number-of-goals))
          (setf result (cons (get-numbers2 cmd (subseq cmd 0 (or pos_space pos_dot))
                                           (get-number-of-goals)
                                           ts current-level (if (< ng ng2) 1 0))
                             result))

          (setf current-level (1+ current-level)))))

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
        (current-level 1)
        (pre           -1)
        (post          (proof-queue-or-locked-end)))
    (goto-char post)
    (test-msg (format "FINAL %s\nPOST %s\nPOINT %s" final post (point)))
    (condition-case nil
        (while (and (>  post   pre)
                    (< (point) final)
                    (not (equal 0 proof-shell-proof-completed)))
          (export-theorem)
          (setq pre  post)
          (setq post (proof-queue-or-locked-end))
          (test-msg (format "PREAFTER %s\nPOSTAFTER %s\nPROOFAFTER %s\nPOINTAFTER %s"
                            pre post (proof-queue-or-locked-end) (point))))
      (end-of-buffer)))
  (setf saved-theorems (remove-nil-cases))
  (test-msg (format "Writing hypotheses %S" proof-hypotheses))
  (write-hypotheses)
  (test-msg (format "Written hypotheses to %S" hypotheses-file)))

(defun remove-nil-cases ()
  (do ((all-theorems  saved-theorems (cdr all-theorems))
       (keep-theorems nil))
      ((endp all-theorems)
       keep-theorems)
    (let ((theorem (car all-theorems)))
      (unless (or (equal   (car theorem) nil)
                  (string= (car theorem) ""))
        (setf keep-theorems (append (list theorem) keep-theorems))))))
