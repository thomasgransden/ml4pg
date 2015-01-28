;; Unit tests for ML4PG, mainly exercising high-level, complex functions.
;; As much as possible, these functions should be broken down into simple,
;; pure functions, with separate tests.

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

(defun generate-and-run (func)
  `(lambda ()
     (let (str (funcall (gen-coq-correct-theorem)))
       (list str
             (ml4pg-load-and-extract-info str ',func)))))

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

(test-with save-numbers-empty
  "savenumbers"
  nil
  (lambda () (list (ml4pg-load-and-extract-info "" 'save-numbers))))

(test-with save-numbers-theorem
  ""
  (generate-and-run 'save-numbers)
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
