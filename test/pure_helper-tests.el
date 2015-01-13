(test-with flatten
           "Turn values into 1D lists"
           (lambda ())
           (lambda ()
             (should (equal (flatten nil) nil))
             (should (equal (flatten 10) '(10)))
             (should (equal (flatten '(1 2 3)) '(1 2 3)))
             (should (equal (flatten '((1 2) (((3) 4 (5 (6 7))) 8) 9))
                            '(1 2 3 4 5 6 7 8 9)))))

(test-with between-spaces
           "Test between-spaces, for extracting Coq names"
           (lambda () (gen-list 'gen-nonempty-string (+ 3 (random 255))))
           (lambda (&rest in-strs)
             (let* ((str  (mapconcat 'identity in-strs " "))
                    (strs (split-string str " ")))
               (should (equal (between-spaces "foo bar baz") "bar"))
               (should (equal (between-spaces str) (nth 1 strs))))))

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

(test-with after-space
           "Finding the position of the text after a single space"
           (lambda () (list (replace-in-string " " "" (gen-nonempty-string))
                            (gen-nonempty-string)))
           (lambda (start end)
             (should (equal (after-space (concat start " " end))
                            (1+ (length start))))))

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
