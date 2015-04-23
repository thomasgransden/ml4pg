;; Unit tests for ML4PG, mainly exercising high-level, complex functions.
;; As much as possible, these functions should be broken down into simple,
;; pure functions, with separate tests.

(test-with pg-available
  "Test ProofGeneral is available"
  nil
  (lambda ()
    (should (fboundp 'coq-mode))
    (should (fboundp 'coq-build-prog-args))))

(test-with lookup-type-id
  "Looks up types in an assoc list"
  (list-of (gen-types-id))
  (lambda (alist)
    (dotimes (n (length alist))
      (let* ((type (car (nth n alist)))
             (id   (cdr (assoc type alist))))
        (should (equal (lookup-type-id alist type)
                                id))))))

(test-with get-type-id
  "Extracts a type from a string of Coq and looks up its ID"
  (lambda () (mapcar (apply-partially 'replace-regexp-in-string "[:\n ]" "")
                     (funcall (list-of (gen-nonempty-string)
                                       (gen-nonempty-string)))))
  (lambda (name type)
    (should (equal (get-type-id-aux (concat name " : " type " "))
                   type))))

(test-with append-hyp
  "Check we can append to the hypothesis"
  (list-of (gen-list (gen-string)) (gen-string))
  (lambda (hyp s)
    (let ((hypothesis hyp))
      (append-hyp s)
      (should (equal hypothesis (append hyp s))))))

(test-with remove-if-empty
  "Test removing empty theorems"
  nil
  (lambda ()
    (let ((thms '(("a") ("b") ("") ("d") ("") ("") ("g") ("")))
          (ids  '((6 3 3) (8) nil (2) (1 7 6 6) nil (4 5 3))))
      (should (equal (remove-if-empty-aux ids thms)
                     '((4) nil (7 1) (2) nil nil nil))))))

(test-with extract-defs-empty
  "Test extracting definitions from an empty buffer"
  (lambda ()
    (list (ml4pg-load-and-extract-info "" 'dependencygraph-defs-aux)))
  (lambda (result)
    (should (tree-of-numbers result))
    (process-with-cmd "dot" (clusterofseveral-pure result
                                                   tables-definitions
                                                   number-of-defs))))

(test-with extract-defs-theorem
  "Try extracting definitions given a single theorem."
  (generate-and-run 'dependencygraph-defs-aux)
  (lambda (str result)
    ))

(test-with dependencygraph-statements-empty
  "statements"
  nil
  (lambda ()
    (list (ml4pg-load-and-extract-info ""
                                       'dependencygraph-statements-aux))))

(test-with dependencygraph-statements-theorem
  "Try extracting statements given a single theorem"
  (generate-and-run 'dependencygraph-statements-aux)
  (lambda (str result)
    ))

(test-with dependencygraph-proof-empty
  "proofs"
  nil
  (lambda ()
    (list (ml4pg-load-and-extract-info "" 'dependencygraph-proof-aux))))

(test-with dependencygraph-proof-theorem
  "Proof, given a theorem"
  (generate-and-run 'dependencygraph-proof-aux)
  (lambda (str result)
    ))

(test-with showtreegraphthm-empty
  "tree"
  nil
  (lambda ()
    (ml4pg-load-and-extract-info ""
                                 (lambda ()
                                   (showtreegraphthm-aux "")))))

(test-with showtreegraphthm-empty
  "show tree graph of theorems, with no theorems"
  (lambda ()
    (let* ((name (funcall (gen-coq-name)))
           (str  (funcall (gen-coq-correct-theorem (gen-const name)))))
      (list name
            str
            (ml4pg-load-and-extract-info str
                                         `(lambda ()
                                            (showtreegraphthm-aux ,name))))))
  (lambda (name str result)
    (should t)))

(test-with show-cluster-bis-empty
  "clusters"
  nil
  (lambda () 'show-clusters-bis))

(test-with show-cluster-bis-theorem
  "clusters with theorem"
  (generate-and-run 'show-clusters-bis)
  (lambda (str result)
    ))

(test-with cluster-definitions-empty
  "clusterdefs"
  nil
  (lambda () (list (ml4pg-load-and-extract-info "" 'cluster-definitions))))

(test-with cluster-definitions-theorem
  "clusterdefs, given a theorem"
  (generate-and-run 'cluster-definitions)
  (lambda (str result)
    ))

(test-with show-clusters-of-theorem-empty
  "cluster theorems"
  nil
  (lambda () (list (ml4pg-load-and-extract-info "" 'show-clusters-of-theorem))))

(test-with show-clusters-of-theorem-theorem
  "Clusters, given a theorem"
  (generate-and-run 'show-clusters-of-theorem)
  (lambda (str result)
    ))

(defun test-save-numbers ()
  (unwind-protect
      (save-numbers-mkdir "coq"
                          'extract-names2
                          'extract-feature-theorems
                          "test/"
                          t)
    (delete-directory (concat home-dir "libs/coq/test") t)))

(test-with save-numbers-empty
  "savenumbers"
  nil
  (lambda () (list (ml4pg-load-and-extract-info "" 'test-save-numbers))))

(test-with save-numbers-theorem
  ""
  (generate-and-run 'test-save-numbers)
  (lambda (str result)
    ))

(test-with exported-libraries-empty
  "clusterlibs"
  nil
  (lambda () (list (ml4pg-load-and-extract-info "" 'exported-libraries))))

(test-with exported-libraries-theorem
  ""
  (generate-and-run 'exported-libraries)
  (lambda (str result)
    ))

(test-with ml4pg-mode-auto
  "Does ml4pg-mode activate when loading a .v file?"
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (should (equal major-mode 'coq-mode))))))

(test-with ml4pg-example-is-valid
  "Does Coq accept ml4pg.v?"
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (proof-process-buffer)
                        (should
                         (string= ""
                                  (replace-regexp-in-string
                                   "[\s\n]"
                                   ""
                                   (buffer-substring (proof-queue-or-locked-end)
                                                     (point-max)))))))))

(test-with extract-example-info
  "Extract info from ml4pg.v"
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (goto-char (point-max))
                        (extract-info-up-to-here)))))

(test-with example-dependencygraph-defs
  ""
  nil
  (lambda ()
    (should nil)
    ;; (with-coq-example (lambda ()
    ;;                     (dependencygraph-defs)))
    ))

(test-with example-dependencygraph-statements
  ""
  nil
  (lambda ()
    (should nil)
    ;; (with-coq-example (lambda ()
    ;;                     (dependencygraph-statements)))
    ))

(test-with example-dependencygraph-prof
  ""
  nil
  (lambda ()
    (should nil)
    ;; (with-coq-example (lambda ()
    ;;                     (dependencygraph-proof)))
    ))

(test-with extract-coq-names
  "Test extracting Coq names from vernacular strings"
  (compose (lambda (ns)
             (let ((proofs (mapcar (lambda (n)
                                     (funcall (gen-coq-correct-theorem
                                                 (gen-const n))))
                                   ns)))
               (list ns (join-strings proofs "\n"))))
           (gen-list (gen-coq-name)))
  (lambda (names str)
    (let ((got (extract-coq-names-from str)))
      (should (equal (length got) (length names)))
      (dolist (name got)
        (should (member name names)))
      (dolist (name names)
        (should (member name got))))))

(test-with example-showtreegraphthm
  ""
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (dolist (thm (extract-coq-names-from (buffer-string)))
                          (showtreegraphthm-aux thm))))))

(test-with example-show-clusters-bis
  ""
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (show-clusters-bis)))))

(test-with example-cluster-definitions
  ""
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (cluster-definitions)))))

(test-with example-show-clusters-of-theorem
  ""
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (show-clusters-of-theorem)))))

(test-with example-save-numbers
  ""
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (test-save-numbers)))))

(test-with example-exported-libraries
  ""
  nil
  (lambda ()
    (with-coq-example (lambda ()
                        (exported-libraries)))))
