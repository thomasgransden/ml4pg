;; Unit tests for ML4PG

;; We only focus on plain Coq for now. Feel free to add SSReflect tests!

;; Test string helper functions

(test-with between-spaces
           "Test between-spaces, for extracting Coq names"
           (lambda () (gen-list 'gen-nonempty-string (+ 3 (random 255))))
           (lambda (&rest in-strs)
             (let* ((str  (mapconcat 'identity in-strs " "))
                    (strs (split-string str " ")))
               (should (equal (between-spaces "foo bar baz") "bar"))
               (should (equal (between-spaces str) (nth 1 strs))))))

(test-with after-space
           "Finding the position of the text after a single space"
           (lambda () (list (replace-in-string " " "" (gen-nonempty-string))
                            (gen-nonempty-string)))
           (lambda (start end)
             (should (equal (after-space (concat start " " end))
                            (1+ (length start))))))

(test-with first-dot
           "Test looking for '.' in strings"
           (lambda () (list (replace-in-string "." "" (gen-nonempty-string))
                            (gen-string)))
           (lambda (start end)
             (should (equal (first-dot (concat start "." end))
                            (length start)))))

(test-with pos-to-dot
           "Test chopping up to dots"
           (lambda () (list (gen-list 'gen-nonempty-string 2)))
           (lambda (in-strs)
             (let* ((str  (mapconcat (lambda (c) (if (equal c ?.) ?x c))
                                     in-strs
                                     "."))
                    (strs (split-string str (regexp-quote ".")))
                    (n    (random (length (car strs)))))
               (should (equal (pos-to-dot str n)
                              (subseq (car strs) n))))))

(test-with take-30
           "Should extract 30 items from a list"
           (lambda () (list (gen-list 'gen-string (+ 30 (random 255)))))
           (lambda (l)
             (should (equal (length (take-30 l)) 30))
             (dotimes (n 30)
               (should (equal (nth n (take-30 l))
                              (nth n l))))))

(test-with find-max-length
           "Finds the length of the longest saved theorem"
           (lambda () (list (gen-list 'gen-string) (+ 256 (random 256))))
           (lambda (thms n)
             (let ((thm (gen-string n)))
               (should (equal (find-max-length (cons thm thms)) n))
               (should (equal (find-max-length (list "foo" "bizzle" "boop")) 6)))))

(test-with lookup-type-id
           "Looks up types in an assoc list"
           (lambda () (list (gen-types-id)))
           (lambda (alist)
             (dotimes (n (length alist))
               (let* ((type (car (nth n alist)))
                      (id   (cdr (assoc type alist))))
                 (should (equal (lookup-type-id alist type)
                                id))))))

(test-with get-type-id
           "Extracts a type from a string of Coq and looks up its ID"
           (lambda () (list (replace-regexp-in-string "[:\n ]" "" (gen-nonempty-string))
                            (replace-regexp-in-string "[:\n ]" "" (gen-nonempty-string))))
           (lambda (name type)
             (should (equal (get-type-id-aux (concat name " : " type " "))
                            type))))

;; The meaty functions

(test-with append-to
           "Check we can append to variables"
           (lambda () (list (gen-string) (gen-list 'gen-string)))
           (lambda (s l)
             (append-to l s)
             (should (member s l))))

(test-with append-hyp
           "Check we can append to the hypothesis"
           (lambda () (list (gen-list 'gen-string) (gen-string)))
           (lambda (hyp s)
             (let ((hypothesis hyp))
               (append-hyp s)
               (should (equal hypothesis (append hyp s))))))

(test-with first-space
           "Check we can find the first space in a string"
           (lambda () (list (replace-in-string " " "x" (gen-nonempty-string))
                            (gen-nonempty-string)))
           (lambda (s1 s2)
             (should (equal (first-space (concat s1 " " s2)) (length s1)))))

(test-with str-between
           "Test extracting strings"
           (lambda () (let* ((char (gen-char))
                             (strs (concat (gen-nonempty-string) char
                                           (gen-nonempty-string) char
                                           (gen-nonempty-string)))
                             (bits (split-string strs (regexp-quote char) t)))
                        (list char
                              (nth 0 bits)
                              (nth 1 bits)
                              (nth 2 bits))))
           (lambda (c s1 s2 s3)
             (should (equal (str-between (concat s1 c s2 s3) c s3) s2))))

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
