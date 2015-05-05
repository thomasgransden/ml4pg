;;; This is the feature vector extraction file for SSReflect

;; Different variables which are used to store information about
;; the numbers associated with tactics, rewrite rules, types, ...

(defvar hypothesis       nil)
(defvar saved-theorems   nil)
(defvar goal-level-temp  nil)
(defvar tactic-level     nil)
(defvar proof-tree-level nil)

;; Variables to store the different values associated with the tactics, the
;; types or the rewrite rules

(defvar tactic_id '(("move"    . 1)
                    ("case"    . 2)
                    ("elim"    . 3)
                    ("apply"   . 4)
                    ("apply/"  . 5)
                    ("move/"   . -5)
                    ("case/"   . 6)
                    ("rewrite" . 7)
                    ("exists"  . 8)
                    ("[]"      . 0)
                    ("exact"   . 9)))

(message "FIXME: 'apply', 'case', etc. match core ELisp function names!")
(defvar move    nil)
(defvar case    nil)
(defvar elim    nil)
(defvar apply   nil)
(defvar apply/  nil)
(defvar move/   nil)
(defvar case/   nil)
(defvar rewrite nil)
(defvar exists  nil)
(defvar done    nil)
(defvar exact   nil)

(defvar types_id '(("nat"  . -2)
                   ("Prop" . -4)
                   ("bool" . -3)
                   ("T"    . -1)
                   ("seq"  . -5)))

(defvar types_id_n -6)
(defvar views_id nil)
(defvar theorems_id nil)

(defvar top-symbol-id '(("forall"  . 5)
                        ("@eq"     . 6)
                        ("and"     . 4)
                        ("iff"     . 8)
                        ("or"      . 3)
                        ("is_true" . 2)
                        ("reflect" . 9)))

(defvar top-symbol-n  10)
(defvar add_to        0.1)
(defvar start         100)
(defvar start_view    101)
(defvar start_thm     101)
(defvar init          0)
(defvar current-level 1)
(defvar dot-level     nil)

;;; Proof tree levels

(defvar tdl1 nil)
(defvar tdl2 nil)
(defvar tdl3 nil)
(defvar tdl4 nil)
(defvar tdl5 nil)

(defun add-info-to-level-aux (info list)
  "Concatenates numbers together, if they're non-zero"
  (if list
      (do ((temp   list (cdr temp))
           (temp1  info (cdr temp1))
           (result nil))
          ((endp temp) result)
        (append-to result
                   (cond ((= (car temp)  0) (car temp1))
                         ((= (car temp1) 0) (car temp))
                         (t                 (string-to-number (format "%s%s"
                                                                      (car temp)
                                                                      (car temp1)))))))
      info))

(defun add-info-to-level (info level)
  (case level
    (1 (setf tdl1 (add-info-to-level-aux info tdl1)))
    (2 (setf tdl2 (add-info-to-level-aux info tdl2)))
    (3 (setf tdl3 (add-info-to-level-aux info tdl3)))
    (4 (setf tdl4 (add-info-to-level-aux info tdl4)))
    (5 (setf tdl5 (add-info-to-level-aux info tdl5)))))

;;; Main function of this file, it is in charge of extracting the
;;; information associated with a theorem

(defun export-theorem ()
  (interactive)
  (setf tdl1          nil
        tdl2          nil
        tdl3          nil
        tdl4          nil
        tdl5          nil
        move          nil
        case          nil
        elim          nil
        apply         nil
        apply/        nil
        move/         nil
        case/         nil
        rewrite       nil
        exists        nil
        done          nil
        exact         nil
        current-level 1
        dot-level     nil
        hypothesis    nil
        goal-level    nil)
  (when (equal init 0)
    (read-lemmas)
    (read-views)
    (setq init 1))
  (export-theorem-aux nil nil)
  (send-coq-cmd (format "Unset Printing All")))

(defun remove-jumps-aux (string res)
  (let ((jump (search nl string)))
    (if jump
        (remove-jumps-aux (subseq string (1+ jump)) (concat res (subseq string 0 jump)))
        (concat res string))))

(defun remove-jumps (string)
  (message "WARNING: using the remove-jumps from ssreflect/feature_extraction_2.el")
  (remove-jumps-aux string ""))

(defun get-type-id (object)
  "A function to obtain the type associated with an object"
  (if (string= "(" (subseq object 0 1))
      -4
      (let* ((a         (remove-jumps (send-coq-cmd (concat "Check " object))))
             (pos_jump  (search nl  a :start2 (+ 2 (search " "  a))))
             (pos_space (search " " a :start2 (+ 2 (search ": " a))))
             (suba      (subseq a (+ 2 (search ": " a)) (or pos_space pos_jump)))
             (type      (cdr (assoc suba types_id))))
        (or type
            (progn (append-to types_id (cons suba types_id_n))
                   (setf types_id_n (1- types_id_n))
                   (1+ types_id_n))))))

(defun get-type-id2 (object)
  (let* ((a         (send-coq-cmd (concat "Check " object)))
         (pos_jump  (search nl  a :start2 (+ 2 (search " "  a))))
         (pos_space (search " " a :start2 (+ 2 (search ": " a))))
         (suba      (subseq a (+ 2 (search ": " a)) (or pos_space pos_jump)))
         (type      (cdr (assoc suba types_id))))
    (or type
        (progn (append-to types_id (cons suba types_id_n))
               (setf types_id_n (1- types_id_n))
               (1+ types_id_n)))))


;; A function to obtain the value of a top symbol

(defun get-top-symbol ()
  (send-coq-cmd (format "Set Printing All"))
  (let* ((res        (send-coq-cmd (format "Focus")))
         (res2       (subseq res (+ 32 (search "============================" res))))
         (fst-symbol (subseq res2 0 (search " " res2))))
    (cond ((search "->" res2) 7)
          (t (let ((is (assoc fst-symbol top-symbol-id)))
               (if is
                   (cdr is)
                   (progn (append-to top-symbol-id
                                     (cons fst-symbol top-symbol-n))

                          (setf top-symbol-n (1+ top-symbol-n))
                          (1- top-symbol-n))))))))

;; In some cases the intro tactic does not have parameters, the following function
;; obtain the type of the object introduced with the intro tactic in those cases
;; Sobra
(defun get-obj-intro ()
  (proof-undo-last-successful-command)
  (let* ((obj    (send-coq-cmd (format "Show Intro")))
         (object (subseq obj 0 (search nl obj))))
    (proof-assert-next-command-interactive)
    (append-to hypothesis object)
    (get-type-id object)))

(defun extract-params (seq res)
  (let ((pos_space (search " " seq))
        (pos_jump  (search nl seq)))
    (if pos_space
        (extract-params (subseq seq (+ 1 pos_space)) (cons (subseq seq 0 pos_space) res))
        (reverse (cons (subseq seq 0 pos_jump) res)))))

;; Given a list of objects, it obtains the value associated with their types

(defun get-types-list (list res)
  (if (endp list)
      (* -1 res)
      (if (search "_" (car list))
          (get-types-list (cdr list) res)
          (get-types-list (cdr list) (+ res
                                        (* -1
                                           (get-type-id (car list))
                                           (expt 10 (1- (length list)))))))))

(defun get-types-list-exists (list res)
  (if (endp list)
      (* -1 res)
    (get-types-list-exists (cdr list) (+ (* -1 (get-type-id2 (car list)) (expt 10 (- (length list) 1))) res))))

;; To obtain the number of tactics applied

(defun get-number-list (list)
  (if (endp list)
      0
    (+ (expt 10 (- (length list) 1))  (get-number-list (cdr list)))))

(defun get-number-list2 (list n)
  (if (endp list)
      0
    (+ (* n (expt 10 (- (length list) 1)))  (get-number-list2 (cdr list) n))))

;; To obtain the value associated with top symbol in the case of move

(defun get-top-symbols-list (len res)
  (if (= len 0)
      res
    (let ((gs (get-top-symbol))
      (ps (send-coq-cmd (format "intro"))))
      (+ (get-top-symbols-list (- len 1) (+ (* gs (expt 10 (- len 1))) res))))))

(defun get-top-symbols-seq (seq res)
  (if (endp seq)
      res
    (let ((gs (get-top-symbol))
      (ps (send-coq-cmd (format (concat "intro " (car seq))))))
      (+ (get-top-symbols-seq (cdr seq) (+ (* gs (expt 10 (- (length seq) 1))) res))))))

;; To obtain the value associated with a theorem

(defun search-in-hyp (obj hyp)
  (if (endp hyp)
      nil
    (if (string= obj (car hyp))
    t
      (search-in-hyp obj (cdr hyp)))))

;;; Auxiliary functions

(defun remove=> (string)
  (replace-regexp-in-string "=>" "" string))

(defun extract-views (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (when (and (string= (subseq (car temp) 0 1) "/")
               (not (string= (car temp) "//"))
               (not (string= (car temp) "/="))
               (not (string= (car temp) "//=")))
      (append-to temp2 (if (string= (subseq (car temp) 0 2) "/(")
                           (subseq (car temp) 2 (search " " (car temp)))
                           (subseq (car temp) 1))))))

(defun extract-rewrites (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (when (or (search "->" (car temp)) (search "<-" (car temp)))
      (append-to temp2 (car temp)))))

(defun extract-simplifications (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (when (or (string= (car temp) "//")
              (string= (car temp) "/=")
              (string= (car temp) "//="))
      (append-to temp2 (car temp)))))

(defun compute-value-simpl (list)
  (list 0 (length list) 0 0))


(defun extract-views-id (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) temp2)
    (if (assoc (car temp) views_id)
        (setf temp2 (concat temp2 (format "%s" (cdr (assoc (car temp) views_id))) ))
        (progn (setf start_view (+ start_view 1))
               (save-view (car temp) start_view)
               (setf views_id
                     (append views_id (list (cons (car temp) start_view))))
               (setf temp2 (concat temp2 (format "%s" (cdr (assoc (car temp) views_id)))))))))



(defun compute-values-rewrite-tactic (list)
  (do ((temp (extract-real-params list) (cdr temp))
       (temp2 ""))
      ((endp temp) (string-to-number temp2))
      (let* ((obj1 (if (string= "-" (subseq (car temp) 0 1)) (subseq (car temp) 1) (car temp)))
         (obj (if (string= "(" (subseq obj1 0 1)) (subseq obj1 1 (search " " obj1)) obj1)))
      (if (assoc obj theorems_id)
      (setf temp2 (concat temp2 (format "%s" (cdr (assoc obj theorems_id)))) )
    (progn (setf start_thm (+ start_thm 1))
           (save-lemma obj start_thm)
           (setf theorems_id
             (append theorems_id (list (cons obj start_thm))))
           (setf temp2 (concat temp2 (format "%s" (cdr (assoc obj theorems_id))))))))))


(defun compute-values-apply-tactic (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) (string-to-number temp2))
      (let ((obj (if (string= "(" (subseq (car temp) 0 1)) (subseq (car temp) 1) (car temp))))
    (if (member obj hypothesis)
        (setf temp2 (concat temp2 "1"))
      (if (assoc obj theorems_id)
          (setf temp2 (concat temp2 (format "%s" (cdr (assoc obj theorems_id)))) )
        (progn (setf start_thm (+ start_thm 1))
           (setf theorems_id
             (append theorems_id (list (cons obj start_thm))))
           (setf temp2 (concat temp2 (format "%s" (cdr (assoc obj theorems_id)))))))))))

(defun compute-value-views-move (list)
  (list (* -1 (get-number-list2 list 5)) (length list) (* -1 (get-number-list2 list 4)) (string-to-number (extract-views-id list))))

(defun compute-value-views-apply (list)
  (list  (get-number-list2 list 5) (length list) (* -1 (get-number-list2 list 4)) (string-to-number (extract-views-id list))))

(defun compute-value-views-case (list)
  (list  (get-number-list2 list 6) (length list) (* -1 (get-number-list2 list 4)) (string-to-number (extract-views-id list))))

(defun compute-value-views-exact (list)
  (list  (get-number-list2 list 9) (length list) (* -1 (get-number-list2 list 4)) (string-to-number (extract-views-id list))))

(defun compute-value-rewrites (list)
  (list (get-number-list2 list 7) (length list) (* -1 (get-number-list2 list 4)) (get-number-list list)))

(defun occurrences (c string)
  (do ((temp string)
       (n 0))
      ((not (search c temp)) n)
    (progn (setf n (1+ n))
           (setf temp (subseq temp (1+ (search c temp)))))))

(defun remove-squared-parenthesis2 (string)
  (do ((temp  string)
       (temp2 ""))
      ((= (length temp) 0) temp2)
    (if (or (string= (subseq temp 0 1) "[")
            (string= (subseq temp 0 1) "]")
            (string= (subseq temp 0 1) "|"))
        (setf temp (subseq temp 1))
        (progn (setf temp2 (concat temp2 (subseq temp 0 1)))
               (setf temp (subseq temp 1))))))

(defun extract-params4 (cmd)
  (let* ((res (extract-params2 (remove-squared-parenthesis2 cmd) nil))
         (res1 (remove-empties res)))
    (put-together-parenthesis res1)))

;;; The following functions provide the numbers associated with a concrete tactic

(defun numbers-move-aux (cmd)
  (extract-params3 (remove=> (subseq cmd (+ 2 (search "=>" cmd))))))

(defun numbers-move=> (cmd top level)
  (let* ((params        (numbers-move-aux         cmd))
         (views         (extract-views            params))
         (simpl         (extract-simplifications  params))
         (rewrites      (extract-rewrites         params))
         (rewrites-nums (compute-value-rewrites   rewrites))
         (simpl-nums    (compute-value-simpl      simpl))
         (views-nums    (compute-value-views-move views))
         (real-params   (extract-real-params      params)))
    (setf hypothesis (append hypothesis real-params))
    (let* ((types-params (get-types-list real-params 0)))
      (add-info-to-level (list (get-types-list real-params 0)
                               0 0 0 0 0 0 0 0 0 0 0 0)
                         level)
      (append-to move (list (get-types-list (when real-params
                                              (list (car real-params)))
                                            0)
                            (get-types-list (cdr real-params) 0)
                            (* -1 (get-number-list real-params))
                            top))
      (let ((result (append (list (list
                                   (get-number-list2 real-params
                                                     (cdr (assoc "move" tactic_id)))
                                   (length real-params)
                                   types-params
                                   (* -1 (get-number-list real-params))))
                            (when simpl    (list simpl-nums))
                            (when views    (list views-nums))
                            (when rewrites (list rewrites-nums)))))
        result))))

(defun numbers-move/ (cmd top level)
  (let* ((params        (extract-params3 (remove=> (subseq cmd 4)) ))
         (views         (extract-views params))
         (simpl         (extract-simplifications params))
         (rewrites      (extract-rewrites params))
         (rewrites-nums (compute-value-rewrites rewrites))
         (simpl-nums    (compute-value-simpl simpl))
         (views-nums    (compute-value-views-move views))
         (real-params   (extract-real-params params))
         (foo           (setf hypothesis (append hypothesis real-params)))
         (foo3          (add-info-to-level (list 0 0 0 0 0 (nth 2 views-nums) 0 0 0 0 0 0 0) level))
         (foo2          (append-to move/ (list -4  (* -4 (get-number-list real-params))  (nth 3 views-nums) top)))
         (types-params  (get-types-list real-params 0)))
    (append (list views-nums)
            (when real-params
              (list (list (get-number-list2 real-params
                                            (cdr (assoc "move" tactic_id)))
                          (length real-params)
                          types-params
                          (* -1 (get-number-list real-params)))))
            (when simpl    (list simpl-nums))
            (when rewrites (list rewrites-nums)))))

(defun numbers-move: (cmd top level)
  (let* ((params        (extract-params3 (subseq cmd (+ 1 (search ":" cmd)))) )
         (views         (extract-views params))
         (simpl         (extract-simplifications params))
         (rewrites      (extract-rewrites params))
         (rewrites-nums (compute-value-rewrites rewrites))
         (simpl-nums    (compute-value-simpl simpl))
         (views-nums    (compute-value-views-move views))
         (real-params   (extract-real-params params))
         (types-params  (get-types-list real-params 0)))
    (add-info-to-level (list (get-types-list real-params 0) 0 0 0 0 0 0 0 0 0 0 0 0)
                       level)
    (append-to move (list (get-types-list (when real-params
                                            (list (car real-params)))
                                          0)
                          (get-types-list (cdr real-params) 0)
                          (* 1 (get-number-list real-params))
                          top))
    (append (list (list (* -1 (get-number-list2 real-params
                                                (cdr (assoc "move" tactic_id))))
                        (length real-params)
                        types-params
                        (* -1 (get-number-list real-params))))
            (when views    (list views-nums))
            (when simpl    (list simpl-nums))
            (when rewrites (list rewrites-nums)))))

(defun numbers-move< (cmd top level)
  (let* ((result  (list (compute-value-rewrites (list 1)))))
    (add-info-to-level (list 0 0 0 0 0 0 0 top 0 0 0 0 0) level)
    (append-to rewrite (list 4 0 1 top))
    result))

(defun numbers-apply: (cmd top level)
  (if (string= cmd "apply")
      (list (list (cdr (assoc "apply" tactic_id)) 1 0 0))
      (let ((moves  (search "=>" cmd))
            (cmdidx (+ 1 (or (search ":" cmd) (search " " cmd)))))
        (add-info-to-level (list 0 0 0 100 0 0 0 0 0 0 0 0 0) level)
        (append-to apply (list -4  0 100 top))
        (if moves
            (let* ((args0      (extract-params4 (subseq cmd (+ 2 moves))))
                   (simpl      (extract-simplifications args0))
                   (simpl-nums (compute-value-simpl simpl))
                   (args       (extract-real-params args0)))
              (append (list (list (cdr (assoc "apply" tactic_id))
                                  1
                                  -4
                                  (compute-values-apply-tactic
                                   (extract-real-params
                                    (extract-params3
                                     (subseq cmd cmdidx
                                                 moves))))))
                      (list (list (* -1
                                     (get-number-list2 args
                                                       (cdr (assoc "move" tactic_id))))
                                  (length args)
                                  (get-types-list args 0)
                                  (* -1 (get-number-list args))))
                      (when simpl (list simpl-nums))))
            (list (list (cdr (assoc "apply" tactic_id))
                        1
                        -4
                        (compute-values-apply-tactic
                         (extract-real-params
                          (extract-params3
                           (subseq cmd cmdidx))))))))))

(defun numbers-elim (cmd top level)
  (let* ((moves  (search "=>" cmd))
         (cmdidx (+ 1 (search ":" cmd))))
    (add-info-to-level (list 0 0
                             (get-types-list (list (car (extract-real-params
                                                         (extract-params3 (subseq cmd cmdidx moves)))))
                                             0)
                             0 0 0 0 0 0 0 0 0 0)
                       level)
    (append-to elim (list (get-types-list (list (car (extract-real-params
                                                      (extract-params3 (subseq cmd cmdidx moves)))))
                                          0)
                          0
                          -1
                          top))
    (if moves
        (let* ((args0      (extract-params4 (subseq cmd (+ 2 moves))))
               (simpl      (extract-simplifications args0))
               (simpl-nums (compute-value-simpl simpl))
               (args       (extract-real-params args0)))
          (append (list (list (cdr (assoc "elim" tactic_id))
                              1
                              (get-types-list (extract-real-params
                                               (extract-params3 (subseq cmd cmdidx moves)))
                                              0)
                              -1))
                  (list (list (* -1
                                 (get-number-list2 args
                                                   (cdr (assoc "move" tactic_id))))
                              (length args)
                              (get-types-list args 0)
                              (* -1 (get-number-list args))))
                  (when simpl (list simpl-nums))))
      (list (list (cdr (assoc "elim" tactic_id))
                  1
                  (get-types-list (extract-real-params
                                   (extract-params3 (subseq cmd cmdidx)))
                                  0)
                  -1)))))

(defun numbers-case (cmd top level)
  (if (string= cmd "case")
      (list (list (cdr (assoc "case" tactic_id)) 1 0 0))
      (let* ((moves  (search "=>" cmd))
             (subcmd (extract-params3 (subseq cmd (+ 1 (or (search ":" cmd)
                                                           (search " " cmd))))))
             (subreal (extract-real-params subcmd)))
        (add-info-to-level (list 0
                                 (if subreal
                                     (get-types-list (list (car subreal)) 0)
                                     1)
                                 0 0 0 0 0 0 0 0 0 0 0)
                           level)
        (append-to case (list (if subreal
                                  (get-types-list (list (car subreal)) 0)
                                  1)
                              0
                              -1
                              top))
        (if moves
            (let* ((args0      (extract-params4 (subseq cmd (+ 2 moves))))
                   (simpl      (extract-simplifications args0))
                   (simpl-nums (compute-value-simpl simpl))
                   (args       (extract-real-params args0)))
              (append (list (append (list (cdr (assoc "case" tactic_id)) 1)
                                    (if subcmd
                                        (list (get-types-list subreal 0) -1)
                                        (list 0 0))))
                      (list (list (* -1 (get-number-list2 args
                                                          (cdr (assoc "move" tactic_id))))
                                  (length args)
                                  (get-types-list args 0)
                                  (* -1 (get-number-list args))))
                      (when simpl (list simpl-nums))))
            (list (list (cdr (assoc "case" tactic_id))
                        1
                        (get-types-list subreal 0)
                        -1))))))

(defun numbers-case/ (cmd top level)
  (let* ((params (extract-params4 (separate-/ (remove=> (subseq cmd 5)) "")))
     (views (extract-views params))
     (simpl (extract-simplifications params))
     (rewrites (extract-rewrites params))
     (rewrites-nums (compute-value-rewrites rewrites))
     (simpl-nums (compute-value-simpl simpl))
     (views-nums (compute-value-views-case views))
     (real-params (extract-real-params params))
     (foo (setf hypothesis (append hypothesis real-params)))
     (types-params (get-types-list real-params 0))
     (foo3 (add-info-to-level (list 0 0 0 0 0 0 (nth 2 views-nums) 0 0 0 0 0 0) level))
     (foo2 (setf case/ (append case/ (list (list -4  (/ (nth 2 views-nums) 10)  (nth 3 views-nums) top))))))
    (append (list views-nums)
        (if real-params (list (list (get-number-list2 real-params (cdr (assoc "move" tactic_id))) (length real-params) types-params (* -1 (get-number-list real-params)))))
        (if simpl (list simpl-nums) nil)
        (if rewrites (list rewrites-nums) nil)))
)

(defun separate-/ (string res)
  (let ((pos (search "/" string)))
    (if (not pos)
    (concat res string)
      (cond ((= pos 0) (separate-/ (subseq string (1+ pos)) (concat "/" res (subseq string 0 pos))))
        ((not (string= " " (subseq string (1- pos) pos)))
         (separate-/ (subseq string (1+ pos)) (concat res (subseq string 0 pos) " /")))
        (t (separate-/ (subseq string (1+ pos)) (concat res (subseq string 0 pos))))))))


(defun numbers-apply/ (cmd top level)
  (let* ((params (extract-params4 (separate-/ (remove=> (subseq cmd 5)) "")))
     (views (extract-views params))
     (simpl (extract-simplifications params))
     (rewrites (extract-rewrites params))
     (rewrites-nums (compute-value-rewrites rewrites))
     (simpl-nums (compute-value-simpl simpl))
     (views-nums (compute-value-views-apply views))
     (real-params (extract-real-params params))
     (foo (setf hypothesis (append hypothesis real-params)))
     (types-params (get-types-list real-params 0))
     (foo3 (add-info-to-level (list 0 0 0 0 (nth 2 views-nums) 0 0 0 0 0 0 0 0) level))
     (foo2 (setf apply/ (append apply/ (list (list -4  (/ (nth 2 views-nums) 10)  (nth 3 views-nums) top))))))
    (append (list views-nums)
        (if real-params (list (list (get-number-list2 real-params (cdr (assoc "move" tactic_id))) (length real-params) types-params (* -1 (get-number-list real-params)))))
        (if simpl (list simpl-nums) nil)
        (if rewrites (list rewrites-nums) nil)))
)

(defun numbers-exact (cmd top level)
  (if (string= cmd "exact")
      (list (list (cdr (assoc "exact" tactic_id)) 1 0 0))
  (let* ((params (extract-params3 (cond ((search ":" cmd) (subseq cmd (+ 1 (search ":" cmd))))
                    ((search "/" cmd) (subseq cmd (search "/" cmd)))
                    (t (subseq cmd (+ 1 (search " " cmd)))))))
    (views (extract-views params))
    (views-nums (compute-value-views-exact views))
    (foo3 (add-info-to-level (list 0 0 0 0 0 0 0 0 0 0 100 0 0) level))
    (foo2 (setf exact (append exact (list (list -4  0 100 top))))))
    (if views
    (list views-nums)
    (list (list (cdr (assoc "exact" tactic_id))
          1
          -4
          (compute-values-apply-tactic (extract-real-params params))))))))

(defun numbers-rewrite (cmd top level)
  (let* ((params (extract-params3 (subseq cmd (+ 1 (search " " cmd)))) )
     (views (extract-views params))
     (simpl (extract-simplifications params))
     (simpl-nums (compute-value-simpl simpl))
     (views-nums (compute-value-views-move views))
     (foo3 (add-info-to-level (list 0 0 0 0 0 0 0 (get-number-list2 (cdr params) 4) 0 0 0 0 0) level))
     (foo2 (setf rewrite (append rewrite (list (list -4  (get-number-list2 (cdr params) 4) (compute-values-rewrite-tactic params) top))))))
    (append (list (list (get-number-list2 params (cdr (assoc "rewrite" tactic_id)))
            (length params)
            (get-number-list2 params 4)
            (compute-values-rewrite-tactic params)))
        (if simpl (list simpl-nums) nil)
        ))
)

(defun numbers-exists (cmd top level)
  (let ((moves (search "=>" cmd))
    (foo3 (add-info-to-level (list 0 0 0 0 0 0 0 0 1 0 0 0 0) level))
    (foo2 (setf exists (append exists (list (list 8 0 1 top))))))
    (if (not moves)
    (let* ((params (extract-params3 (subseq cmd 7)) )
           (types-params (get-types-list-exists params 0))
           )
      (list (list (cdr (assoc "exists" tactic_id)) 1 types-params 0)))
      (let* ((args0 (extract-params4 (subseq cmd (+ 2 moves))))
        (simpl (extract-simplifications args0))
        (simpl-nums (compute-value-simpl simpl))
        (args (extract-real-params args0)))
      (append (list (list (cdr (assoc "exists" tactic_id))
              1 (get-types-list-exists  (extract-params3 (subseq cmd 7 moves)) 0) -1))
          (list (list (* -1 (get-number-list2 args (cdr (assoc "move" tactic_id)))) (length args) (get-types-list args 0) (* -1 (get-number-list args))))
          (if simpl (list simpl-nums) nil)))))
  )


(defun numbers-done (cmd top level)
  (progn
    (add-info-to-level (list 0 0 0 0 0 0 0 0 0 top 0 0 0) level)
    (setf done (append done (list (list 0 0 0 top))))
    (list (list (cdr (assoc "[]" tactic_id)) 1 0 0) ) )
)


(defun remove-multiple-spaces (string)
  (let ((d (search "  " string)))
    (if d
    (remove-multiple-spaces (concat (subseq string 0 d) (subseq string (1+ d))))
      string)))

(defun compute-numbers-cmd (cmd top level)
  (let* ((cmd1 (remove-multiple-spaces cmd)))
    (cond ((search "symmetry" cmd) nil)
          ((search "last by" cmd) (compute-numbers-cmd (subseq cmd (+ 3 (search "by" cmd))) top level))
          ((search "first by" cmd) (compute-numbers-cmd (subseq cmd (+ 3 (search "by" cmd))) top level))
          ((string= "try" (subseq cmd 0 2)) (compute-numbers-cmd (subseq cmd (+ 4 (search "try" cmd))) top level))
          ((string= "do" (subseq cmd 0 2)) (compute-numbers-cmd (subseq cmd (cond ((search "!" cmd) (1+ (search "!" cmd)))
                                                                                  ((search "?" cmd) (1+ (search "?" cmd)))
                                                                                  (t (+ 3 (search "do" cmd))))) top level))
          ((search "have" cmd) nil)
          ((or (search "move=>" cmd1) (search "move =>" cmd1)) (numbers-move=> cmd1 top level))
          ((or (search "move:" cmd1) (search "move :" cmd1)) (numbers-move: cmd1 top level))
          ((or (search "move/" cmd1) (search "move /" cmd1)) (numbers-move/ cmd1 top level))
          ((or (search "move<-" cmd1) (search "move->" cmd1) (search "move ->" cmd1) (search "move <-" cmd1)) (numbers-move< cmd1 top level))
          ((or (search "apply/" cmd1) (search "apply /" cmd1)) (numbers-apply/ cmd1 top level))
          ((or (search "apply:" cmd1) (search "apply :" cmd1) (search "apply" cmd1)) (numbers-apply: cmd1 top level))
          ((or (search "elim:" cmd1) (search "elim :" cmd1)) (numbers-elim cmd1 top level))
          ((or (search "case/" cmd1) (search "case /" cmd1)) (numbers-case/ cmd1 top level))
          ((or (search "case:" cmd1) (search "case" cmd1)) (numbers-case cmd1 top level))
          ((or (search "exact" cmd1) (search "exact :" cmd1)) (numbers-exact cmd1 top level))
          ((search "rewrite" cmd1) (numbers-rewrite cmd1 top level))
          ((search "exists" cmd1) (numbers-exists cmd1 top level))
          ((or (search "[]" cmd1) (search "done" cmd1) (search "constructor" cmd1)) (numbers-done cmd1 top level))

          ((string= (subseq cmd1 0 4) "pose") nil)
          ((string= (subseq cmd1 0 3) "set") nil)
          ((string= (subseq cmd1 0 4) "left") nil)
          ((string= (subseq cmd1 0 4) "righ") nil))))

(defun split-command (cmd result end)
  (if (or (string= " " (subseq cmd 0 1))
          (string= "-" (subseq cmd 0 1)))
      (split-command (subseq cmd 1) result end)
      (if (string= "by" (subseq cmd 0 2))
          (split-command (subseq cmd 3) result 1)
          (let ((comma (search ";" cmd)))
            (if comma
                (split-command (subseq cmd (1+ comma))
                               (append result (list (subseq cmd 0 comma)))
                               end)
                (list (append result
                              (list (subseq cmd 0 (1- (length cmd)))))
                      end))))))

(defun add-tactics (tactics end top level)
  (do ((temp tactics (cdr temp))
       (temp2 nil))
      ((endp temp) (if (> end 0) (append temp2 (list (list 0 1 0 0))) temp2))
      (let ((res (compute-numbers-cmd (car temp) top level)))
    (if res (setf temp2 (append temp2 res))))))

;The first value is the tactic, the second one is the number of tactics,
;the third one is the argument type, the fourth one is if the
;argument is a hypothesis of a theorem, the fifth one is the top-symbol
;and the last one the number of subgoals

(defun minus-any (&rest args)
  "Return a hyphen if any arg is negative, or an empty string otherwise"
  (let ((result ""))
    (dolist (arg args result)
      (when (< arg 0) (setf result "-")))))

(defun get-numbers-helper (temp2 elem index take-abs)
    (let ((n1 (string-to-number (nth index temp2)))
          (n2 (nth index elem)))
      (concat (format "%s%s"
                      (minus-any (if take-abs (abs n1) n1)
                                 n2)
                      (abs n1))
              (format "%s" (abs n2)))))

(defun get-numbers (cmd top level)
  (let* ((res (split-command cmd nil 0))
         (tactics (car res))
         (end (cadr res))
         (nums (add-tactics tactics end top level)))
    (when nums
      (let ((temp2 (list (format "%s" (nth 0 (car nums)))
                         (nth 1 (car nums))
                         (format "%s" (nth 2 (car nums)))
                         (format "%s" (nth 3 (car nums))))))
        (dolist (elem (cdr nums) (list (string-to-number (nth 0 temp2))
                                       (nth 1 temp2)
                                       (string-to-number (nth 2 temp2))
                                       (string-to-number (nth 3 temp2))))
          (setf temp2 (list (get-numbers-helper temp2 elem 0 nil)
                            (+ (nth 1 temp2) (nth 1 elem))
                            (get-numbers-helper temp2 elem 2 t)
                            (get-numbers-helper temp2 elem 3 nil))))))))

;; Function to obtain the information just about the goals.

(defun count-seq (item seq)
  (let ((is? (search item seq)))
    (if is?
        (1+ (count-seq item (subseq seq (1+ is?))))
        0)))

(defun get-number-of-goals ()
  (let ((r (send-coq-cmd (format "Show Proof"))))
    (count-seq "?" r)))

(defun flat (ll)
  (unless (endp ll)
    (append (car ll) (flat (cdr ll)))))

;; The following function computes the result of the tactic

(defun digits (n)
  (if (= (mod n 10) 0)
      0
      (1+ (digits (/ n 10)))))

(defun first-digit (n digits)
  (/ n (expt 10 (1- digits))))

(defun rest-of-digits (n digits)
  (- n (* (first-digit n digits) (expt 10 (1- digits)))))

(defun obtain-tactic-result (tactic)
  (do ((temp (cdr tactic) (cdr temp))
       (temp2 (if (endp tactic) (list 0 0 0 0 0)
          (list (first-digit (nth 0 (car tactic)) (digits (nth 0 (car tactic))))
            (* (rest-of-digits (nth 0 (car tactic)) (digits (nth 0 (car tactic)))) (expt 10 (length (cdr tactic))))
            (* (nth 1 (car tactic)) (expt 10 (length (cdr tactic))))
            (nth 2 (car tactic))
            (nth 3 (car tactic))))))
      ((endp temp) temp2)
    (setf temp2 (list (nth 0 temp2)
              (+ (nth 1 temp2) (* (expt 10 (length (cdr temp))) (nth 0 (car temp))))
              (+ (nth 2 temp2) (* (expt 10 (length (cdr temp))) (nth 1 (car temp))))
              (concat (format "%s" (nth 3 temp2)) (format "%s" (nth 2 (car temp))))
              (+ (nth 4 temp2) (nth 3 (car temp)))))))

(defvar useless-terms '("Structure" "Section" "Add Ring" "Hypothesis"
                        "Hypotheses" "Include" "Export" "Parameter" "Axiom"
                        "End" "Notation" "Hint" "Inductive" "Variable"
                        "Implicit" "Import" "Canonical" "Coercion" "Module"
                        "Ltac" "Let" "Opaque" "Bind" "Scope" "Require" "Infix"
                        "Record" "Fact" "Print"))

(defun compute-tactic-value (lst)
  "Concatenate the numbers in LST"
  (if lst
      (let* ((len  (length lst))
             (head (car lst))
             (arg0              (nth 0 head))
             (arg1 (format "%s" (nth 1 head)))
             (hyp  (format "%s" (nth 2 head)))
             (top  (format "%s" (nth 3 head))))
        (dolist (elem
                 (cdr lst)
                 (list arg0
                       (string-to-number arg1)
                       (string-to-number hyp)
                       (string-to-number top)
                       len))
          (setf arg1 (format "%s%s%s" arg1 (nth 0 elem) (nth 1 elem)))
          (setf hyp  (format "%s%s"   hyp  (nth 2 elem)))
          (setf top  (format "%s%s"   top  (nth 3 elem)))))
      (list 0 0 0 0 0)))

(defun compute-tactic-result (name)
  (append (list name) (list (append
    (compute-tactic-value move)
    (compute-tactic-value case)
    (compute-tactic-value elim)
    (compute-tactic-value apply/)
    (compute-tactic-value move/)
    (compute-tactic-value case/)
    (compute-tactic-value rewrite)
    (compute-tactic-value exists)
    (compute-tactic-value done)
    (compute-tactic-value exact)))))

(defun compute-proof-tree-result (name)
  (append (list name) (list (append
    (if tdl1 tdl1 (generate-zeros 13))
    (if tdl2 tdl2 (generate-zeros 13))
    (if tdl3 tdl3 (generate-zeros 13))
    (if tdl4 tdl4 (generate-zeros 13))
    (if tdl5 tdl5 (generate-zeros 13))))))

(defun export-theorem-aux (result name)
  (export-theorem-aux2 result name nil))

(defun export-theorem-otherwise (cmd result name args)
  (add-hypotheses)
  (setf ts  (get-top-symbol))
  (setf ng  (get-number-of-goals))
  (proof-assert-next-command-interactive)
  (setf ng2 (get-number-of-goals))
  (export-theorem-aux2 (cons (append (get-numbers cmd ts current-level)
                                     (list ts)
                                     (list ng2))
                             result)
                       name
                       args)
  (add-info-to-level (list 0 0 0 0 0 0 0 0 0 0 0 ng2 (if (< ng2 ng) 1 0))
                     current-level)
  (setf current-level (1+ current-level)))

(defun export-theorem-aux2 (result name args)
  (let* ((semis   (save-excursion
                    (skip-chars-backward " \t\n"
                                         (proof-queue-or-locked-end))
                    (proof-segment-up-to-using-cache (point))))
         (first   (car semis))
         (comment (nth 0 first))
         (cmd     (nth 1 first))
         (subcmd  (ignore-errors (between-spaces cmd)))
         (ts      nil))
    (when semis
      (cond ((or (string= comment "comment")
                 (is-in-search cmd))
             (export-theorem-comment result name   args))

            ((or (search "Definition" cmd)
                 (search "Fixpoint"   cmd))
             (export-theorem-deffix  result subcmd args))

            ((search "Lemma" cmd)
             (export-theorem-comment result subcmd args))

            ((search "Proof" cmd)
             (export-theorem-comment result name   args))

            ((search "Theorem" cmd)
             (export-theorem-comment result subcmd args))

            ((or (search "Qed."     cmd)
                 (search "Defined." cmd))
             (export-theorem-defined   name result args))

            (t
             (export-theorem-otherwise cmd  result name args))))))

(defun split-feature-vector (name fv)
  (let ((len (1+ (floor (length fv) 30))))
    (dotimes (i len)
      (setf saved-theorems (append saved-theorems
                                   (list (list name (take-30-from fv i))))))))

(defun take-30-from (list row)
  "Chunk LIST into runs of length 30, and return run POS"
  (take-n 30 (drop-n (* row 30) list)))

;;; Functions to save the files

(defun save-file-conventions1 ()
  (interactive)
  (let ((file (read-file-name "Save in file (don't include the extension): ")))
    (progn (with-temp-file (concat file "_goals.csv") (insert (extract-features-1)))
       (with-temp-file (concat file "_tactics.csv") (insert (extract-features-2 tactic-level)))
       (with-temp-file (concat file (format "_summary.txt")) (insert (extract-names))))))

(defun extract-names ()
  (do ((temp saved-theorems (cdr temp))
       (temp2 "")
       (i 1 (1+ i)))
      ((endp temp) temp2)
    (setf temp2 (concat temp2 (format "%s %s\n" i (remove_last_colon (caar temp)))) )))

(defun last-part-of-lists (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (setf temp2 (append temp2 (list (cadar temp))))))

(defun extract-features-1 ()
  (let ((fm (longest-theorem)))
    (do ((temp (last-part-of-lists saved-theorems) (cdr temp))
         (temp2 ""))
        ((endp temp) temp2)
      (setf temp2 (concat temp2
                          (format "%s\n"
                                  (print-list  (take-30 (append (car temp)
                                                                (generate-zeros 30))))))))))

(defun extract-features-2 (list)
  (do ((temp (last-part-of-lists (cdr list)) (cdr temp))
       (temp2 ""))
      ((endp temp) temp2)
    (setf temp2 (concat temp2 (format "%s\n" (print-list (car temp)))))))

(defun generate-zeros (n)
  (do ((i 0 (1+ i))
       (temp nil (cons 0 temp)))
      ((= i n) temp)))

(defun longest-theorem ()
  (find-max-length saved-theorems))

;; Function which extract the info of a theorem up to a concrete point

(defvar tactic-temp     nil)
(defvar proof-tree-temp nil)

(defun extract-info-up-to-here ()
  (interactive)
  (setf move          nil
        case          nil
        elim          nil
        apply         nil
        apply/        nil
        move/         nil
        case/         nil
        rewrite       nil
        exists        nil
        done          nil
        exact         nil
        tactic-temp   nil
        tdl1          nil
        tdl2          nil
        tdl3          nil
        tdl4          nil
        tdl5          nil
        current-level 1
        dot-level     nil)
  (let ((final  (point))
        (result nil)
        (end    nil))
    (search-backward "Proof.") ;; FIXME: what if we don't say "Proof."????
    (proof-goto-point)
    (while (< (point) final)
      (let* ((semis   (save-excursion
                        (skip-chars-backward " \t\n"
                                             (proof-queue-or-locked-end))
                        (proof-segment-up-to-using-cache (point))))
             (comment (caar semis))
             (cmd     (cadar semis))
             (ts      nil))
        (setf ts  (get-top-symbol))
        (setf ng  (get-number-of-goals))
        (proof-assert-next-command-interactive)
        (setf ng2 (get-number-of-goals))
        (when cmd
          (setf result (cons (append (get-numbers cmd ts current-level) (list ts) (list ng2)) result)))
        (add-info-to-level (list 0 0 0 0 0 0 0 0 0 0 0 ng2 (if (< ng2 ng) 1 0)) current-level)
        (setf current-level (1+ current-level))))
    (setf tactic-temp     (cadr (compute-tactic-result     "")))
    (setf proof-tree-temp (cadr (compute-proof-tree-result "")))
    (take-30 (append (flat (reverse result)) (generate-zeros 30) ))
    (send-coq-cmd (format "Unset Printing All"))))

(defun extract-features-1-bis (thm)
  (let ((fm    (longest-theorem))
        (temp2 ""))
    (dolist (elem (append (last-part-of-lists saved-theorems) (list thm)) temp2)
      (setf temp2 (concat temp2 (format "%s\n"
                                        (print-list (take-30 (append elem
                                                                     (generate-zeros 30))))))))))

(defun extract-features-2-bis (thm list)
  (let ((fm    (longest-theorem))
        (temp2 ""))
    (dolist (elem (append (last-part-of-lists (cdr list)) (list thm)) temp2)
      (setf temp2 (concat temp2 (format "%s\n" (print-list elem)))))))

(defun extract-feature-theorems ()
  "Function which extract the information from all the theorems up to a point"
  (interactive)
  (let ((final         (point))
        (current-level 1)
        (last-point    -1))
    (export-theorem)
    (while (and (< (point) final)
                (not (= (point) last-point)))
      (setq last-point (point))
      (export-theorem))))
