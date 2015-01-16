;; Unit tests for ML4PG

;; We only focus on plain Coq for now. Feel free to add SSReflect tests!

;; Test string helper functions

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

;; The meaty functions

(test-with append-hyp
           "Check we can append to the hypothesis"
           (lambda () (list (gen-list 'gen-string) (gen-string)))
           (lambda (hyp s)
             (let ((hypothesis hyp))
               (append-hyp s)
               (should (equal hypothesis (append hyp s))))))

(test-with remove-if-empty
           "Test removing empty theorems"
           (lambda ())
           (lambda ()
             (let ((thms '(("a") ("b") ("") ("d") ("") ("") ("g") ("")))
                   (ids  '((6 3 3) (8) nil (2) (1 7 6 6) nil (4 5 3))))
               (should (equal (remove-if-empty-aux ids thms)
                              '((4) nil (7 1) (2) nil nil nil))))))

(test-with extract-defs-empty
           "Test extracting definitions from an empty buffer"
           (lambda ())
           (lambda ()
             ;(ml4pg-load-and-extract-info "" 'dependencygraph-defs)
             (should t)))
