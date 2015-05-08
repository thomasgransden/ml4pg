;; Test feature extraction

(defun gen-squared-parentheses (gen-num gen-str)
  `(lambda ()
     (let* ((n        (% (funcall ,gen-num) 11))
            (str1     (strip-parens (funcall ,gen-str)))
            (str2     (strip-parens (funcall ,gen-str)))
            (str3     (strip-parens (funcall ,gen-str)))
            (str4     (strip-parens (funcall ,gen-str)))
            (str5     (strip-parens (funcall ,gen-str)))
            (brackets (case n
                        (0  (list ""  ""  ""  ""))
                        (1  (list "[" "]" ""  ""))
                        (2  (list "{" "}" ""  ""))
                        (3  (list "[" "]" "[" "]"))
                        (4  (list "[" "]" "{" "}"))
                        (5  (list "{" "}" "[" "]"))
                        (6  (list "{" "}" "{" "}"))
                        (7  (list "[" "{" "}" "]"))
                        (8  (list "{" "[" "]" "}"))
                        (9  (list "[" "[" "]" "]"))
                        (10 (list "{" "{" "}" "}"))))
            (str      (concat str1 (nth 0 brackets)
                              str2 (nth 1 brackets)
                              str3 (nth 2 brackets)
                              str4 (nth 3 brackets)
                              str5)))
       (list n str1 str2 str3 str4 str5 str))))

(test-with remove-squared-parenthesis
  "Test what remove-squared-parenthesis does"
  (list-of (gen-squared-parentheses (gen-num) (gen-string))
           (gen-string))
  (lambda (args res)
    (let* ((n      (nth 0 args))
           (str1   (nth 1 args))
           (str2   (nth 2 args))
           (str3   (nth 3 args))
           (str4   (nth 4 args))
           (str5   (nth 5 args))
           (str    (nth 6 args)))
      (should (equal (remove-squared-parenthesis str res)
                     (concat res str1 (case n
                                        (0  (concat str2 str3 str4     str5))
                                        (1  (concat      str3 str4     str5))
                                        (2  (concat      str3 str4     str5))
                                        (3  (concat      str3          str5))
                                        (4  (concat      str3          str5))
                                        (5  (concat      str3          str5))
                                        (6  (concat      str3          str5))
                                        (7  (concat                    str5))
                                        (8  (concat                    str5))
                                        (9  (concat           str4 "]" str5))
                                        (10 (concat           str4 "}" str5)))))))))

(defun trace ()
  (debug))

(test-with numbers-move-arrow
  "Test what numbers-move=> does"
  (list-of (gen-coq-name)
           (gen-coq-name)
           (gen-num)
           (gen-num)
           (gen-coq-correct-theorem))
  (lambda (pre post top level thm)
    (let* ((cmd  (concat pre "=>" post)))
      (ml4pg-load-and-extract-info thm
                                   `(lambda ()
                                      (numbers-move=> ,cmd ,top ,level))))))

(test-with remove-arrow
  "Test what remove=> does"
  (list-of (gen-string) (gen-string))
  (lambda (str1 str2)
    (let ((str (concat str1 "=>" str2)))
      (remove=> (subseq str (+ 2 (search "=>" str)))))))

(test-with numbers-move-aux
  "Test part of numbers-move=>"
  (list-of (gen-squared-parentheses (gen-num) (gen-string))
           (gen-string))
  (lambda (args post)
    (let* ((str   (nth 6 args))
           (post2 (strip-parens post))
           (cmd   (concat str "=>" post2)))
      (numbers-move-aux cmd))))

(test-with extract-params3
  "Test calling convention of extract-params3"
  (list-of (gen-squared-parentheses (gen-num) (gen-coq-name)))
  (lambda (args)
    (let ((str (nth 6 args)))
      (extract-params3 str))))

(test-with get-types-list
  "Test calling convention of get-types-list"
  nil
  (lambda ()
    (get-types-list nil 0)))

(test-with get-type-id
  "Test calling convention of get-type-id"
  (list-of (gen-coq-correct-theorem)
           (gen-coq-name))
  (lambda (thm name)
    (ml4pg-load-and-extract-info thm
                                 `(lambda ()
                                    (get-type-id ,name)))))

(test-with export-theorem-aux
  "Test what export-theorem-aux does"
  (list-of (gen-coq-name) (gen-coq-correct-theorem))
  (lambda (str coq)
    (ml4pg-load-and-execute
     coq
     `(lambda ()
        (message "EXPORT RESULT: (%S)" (export-theorem-aux nil ,str))))))

(test-with compute-tactic-result
  "Test what compute-tactic-result does"
  (list-of (gen-string))
  (lambda (str)
    (let ((result (compute-tactic-result str)))
      (should (equal (length result) 2))
      (should (equal (car result) str)))))

(test-with compute-tactic-value-first
  "How compute-tactic-value treats the first element of its argument"
  (list-of (gen-nonempty-list (gen-list (gen-num) (gen-const 4))))
  (lambda (lst)
    (let ((result (compute-tactic-value lst))
          (head   (car lst)))
      (should (equal  (nth 0 result) (nth 0 head)))
      (should (subnum (nth 1 result) (nth 1 head)))
      (should (subnum (nth 2 result) (nth 2 head)))
      (should (subnum (nth 3 result) (nth 3 head))))))

(test-with compute-tactic-value-rest
  "Relation between the input and output of compute-tactic-value"
  (list-of (gen-list (gen-list (gen-num) (gen-const 4))
                     (compose (lambda (x) (+ 2 x)) (gen-num))))
  (lambda (lst)
    (let ((result (compute-tactic-value lst)))
      (should (equal (length lst)    (nth 4 result)))
      (dolist (elem (cdr lst))
        (dolist (bits '((0 1) (1 1) (2 2) (3 3)))
          (should (subnum (nth (cadr bits) result)
                          (nth (car  bits) elem))))))))

(test-with compute-tactic-value-general
  "General invariants of compute-tactic-value results"
  (list-of (gen-list (gen-list (gen-num) (gen-const 4))))
  (lambda (lst)
    (let ((result (compute-tactic-value lst)))
      (should (listp result))
      (should (equal (length result) 5))
      (dolist (elem result)
        (should (numberp elem))))))

(test-with compute-proof-tree-result
  "Test what compute-proof-tree-result does"
  (list-of (gen-string))
  (lambda (str)
    (compute-proof-tree-result str)))

(test-with adddefinition
  "Test what adddefinition does"
  (list-of (gen-coq-name))
  (lambda (name)
    (message "NAME: %s" name)
    (should-not listofdefinitions)
    (let ((pre listofdefinitions)
          (def nil))
      (adddefinition name)
      (should (equal 1 (length listofdefinitions)))
      (message "POST:\n\n%S\n\n" listofdefinitions))))

(test-with addthm
  "Test what addthm does"
  (list-of (gen-string))
  (lambda (str)
    (addthm str)))

(test-with split-feature-vector
  "Test what split-feature-vector does"
  nil
  (lambda ()
          (split-feature-vector)))

(test-with normalize
  "Test what normalize does"
  (list-of (gen-nested-list (gen-num) 2))
  (lambda (lst)
    (let ((result (normalize lst)))
      (message "IN: %S OUT: %S" lst result))))

(test-with max-position
  "Test what max-position does"
  (list-of (gen-nested-list (gen-num) 2))
  (lambda (lst)
    (max-position lst)))

(test-with min-position
  "Test what min-position does"
  (list-of (gen-nested-list (gen-num) 2))
  (lambda (lst)
    (min-position lst)))

(test-with obtain-definition
  "Test that obtain-definition gets a definition from Coq"
  (list-of (gen-coq-name))
  (lambda (name)
    (let ((result (obtain-definition name)))
      (if (search "Error" result)
          (should (equal (remove-whitespace result)
                         (remove-whitespace (format "Error: %s not a defined object."
                                                    name))))
          (progn ;; More likely than you think; eg. I : True
            (should (search ":=" result))
            (should (search name result)))))))

(test-with definition-to-list
  "Test how definition-to-list behaves"
  (list-of (gen-list (compose (lambda (args)
                                (join-strings (cadr args) (case (% (car args) 4)
                                                            (0 "")
                                                            (1 "fix ")
                                                            (2 "let ")
                                                            (3 "fun "))))
                              (list-of (gen-num) (gen-list (gen-string))))
                     (compose (lambda (x) (+ 2 x)) (gen-num))))
  (lambda (terms)
    (message "TERM: %S" terms)
    (let* ((term   (join-strings terms ":="))
           (foo    (message "TERMS: %S" term))
           (result (definition-to-list term)))
      (message "RESULT %S" result))))

(test-with definition-to-list-aux
  "Test definition-to-list-aux"
  (list-of (gen-readable) (gen-readable))
  (lambda (pre post)
    (message "PRE %S POST %S" pre post)
    (let ((result (definition-to-list-aux (concat pre "=" post))))
      (message "RESULT %S" result))))

(test-with definition-to-list-fix
  "Test behaviour of definition-to-list-fix"
  (list-of (gen-readable) (gen-readable))
  (lambda (pre post)
    (message "PRE %S POST %S" pre post)
    (let ((result (definition-to-list-fix (concat pre ":=" post))))
      (message "RESULT %S" result))))

(test-with transform-match
  "Test the behaviour of transform-match"
  (list-of (gen-readable))
  (lambda (str)
    (let* ((result    (transform-match str))
           (printed   (format "%S" result))
           (condensed (replace-regexp-in-string (regexp-quote "()")
                                                "nil"
                                                (remove-whitespace str))))
      (should (listp result))
      (if (equal "" (remove-whitespace str))
          (should (equal printed "nil"))
          (should (equal (remove-whitespace printed)
                         (concat "(" condensed ")")))))))

(test-with transform-match-barred
  "Test how transform-match handles |"
  (list-of (gen-readable) (gen-readable) (gen-readable))
  (lambda (pre mid post)
    (let ((str (concat pre "|" mid "=> " post)))
      (message "INPUT: %S" str)
      (message "OUTPUT: " (transform-match str)))))

(test-with add-parentheses-match0
  "Test behaviour of add-parentheses-match0"
  (list-of (gen-string-without "|"))
  (lambda (str)
    (let ((result (add-parentheses-match0 str)))
      (test-msg (format "BEFORE: %S" str))
      (test-msg (format "AFTER: %S" result))
      (should (equal "(" (subseq                 result  0 1)))
      (should (equal ")" (subseq (string/reverse result) 0 1)))
      (should-not (search ">" result)))))

(test-with take-30
  "Test we can take 30 elements from a list"
  (list-of (gen-list (gen-string)))
  (lambda (lst)
    (let ((result (take-30 lst)))
      (should (<= (length result) 30))
      (if (<= (length lst) 30)
          (should (equal (length result) (length lst)))
          (should (equal (length result) 30)))
      (dolist (elem result)
        (should (member elem lst))))))

(test-with take-30-from-chunks
  "Check recursive property of take-30-from"
  (list-of (gen-list (gen-num)) (compose '1+ (gen-num)))
  (lambda (lst n)
    (should (> n 0))  ;; Due to 1+
    (should (equal (take-30-from            lst      n)
                   (take-30-from (drop-n 30 lst) (1- n))))))

(test-with take-30-from
  "Test take-30-from"
  (list-of (gen-list (gen-num)) (gen-num))
  (lambda (lst n)
    (let ((result (take-30-from lst n)))
      (should (<= (length result) 30))
      (dolist (elem result)
        (should (member elem lst))))))

(test-with remove-bar-constraints
  "Test what remove-bar does"
  (list-of (compose (lambda (strs)
                      (let ((len (length strs)))
                        (if (< len 3)
                            ;; Return as-is
                            strs
                            ;; Join last two with '=> ' and the rest with '|'
                            (join-strings (append (take-n (- len 2) strs)
                                                  (list (concat (nth (- len 2) strs)
                                                                "=> "
                                                                (nth (- len 1) strs))))
                                          "|"))))
                    (gen-list (gen-string-without "|" "=> "))))
  (lambda (barred)
    (let* ((result (remove-bar barred)))
      (should-not (search "|" result))
      (message "IN:\n\n\n%S\n\n\nOUT:\n\n\n%S\n" barred result))))

(test-with remove-bar-slice
  "Test slicing by remove-bar"
  (list-of (gen-list (gen-list (gen-string-without "=> " "|")
                               (gen-const 3))
                     (compose '1+ (gen-num))))
  (lambda (strss)
    (let ((acc-in     "")
          (acc-expect ""))
      (dolist (strs strss)
        (let* ((str1   (nth 0 strs))
               (str2   (nth 1 strs))
               (str3   (nth 2 strs))
               (in     (concat str1 "|" str2 "=> " str3))
               (expect (concat str1 ")> " str3))
               (result ))
          (setf acc-in     (concat acc-in     in))
          (setf acc-expect (concat acc-expect expect))
          (should (equal     expect (remove-bar     in)))
          (should (equal acc-expect (remove-bar acc-in))))))))
