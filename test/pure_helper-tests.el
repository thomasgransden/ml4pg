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
           (strs (string-split str " ")))
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
  (lambda ()
    (let ((prefix (gen-nonempty-string))
           (middle (gen-string-without "."))
           (suffix (gen-nonempty-string)))
      (list (concat prefix middle suffix) middle (length prefix))))
  (lambda (str middle n)
    (should (equal (pos-to-dot str n) middle))))

(test-with first-space
  "Check we can find the first space in a string"
  (lambda () (list (replace-in-string " " "x" (gen-nonempty-string))
                   (gen-nonempty-string)))
  (lambda (s1 s2)
    (should (equal (first-space (concat s1 " " s2)) (length s1)))))

(test-with str-between
  "Test extracting strings"
  (lambda ()
    (let* ((start  (gen-any 'gen-nonempty-string 'gen-char))
           (end    (gen-any 'gen-nonempty-string 'gen-char))
           (prefix (gen-filtered 'gen-nonempty-string
                                 (lambda (x) (not (search start x)))))
           (middle (gen-filtered 'gen-nonempty-string
                                 (lambda (x) (not (search end   x)))))
           (suffix (gen-nonempty-string)))
      (list start end middle (concat prefix start middle end suffix))))
  (lambda (start end desired str)
    (should (equal (str-between str start end) desired))))

(test-with str-up-to
  "Test extracting prefixes of strings"
  (lambda ()
    (let* ((end    (gen-any 'gen-nonempty-string 'gen-char))
           (prefix (gen-filtered 'gen-nonempty-string
                                 (lambda (x) (not (search end x))))))
      (list (concat prefix end (gen-string)) end prefix)))
  (lambda (str end prefix)
    (should (equal (str-up-to str end) prefix))))

;; These tests are pretty similar, so they share a generator
(let ((generator (lambda ()
                   (let* ((sep      (gen-any 'gen-nonempty-string
                                             'gen-char))
                          (not-sep `(lambda (x) (not (search ,sep x))))
                          (bits     (gen-any  (lambda () nil)
                                             `(lambda ()
                                                (gen-list `(lambda ()
                                                             (gen-filtered 'gen-string
                                                                           not-sep)))))))
                     (list (join-strings bits sep) sep (or bits '("")))))))
  `(test-with split-string
     "Test we're using Emacs's built-in string splitting correctly"
     ,generator
     (lambda (str sep bits)
       (let ((case-fold-search nil))
         (should (equal (split-string str (regexp-quote sep)) bits)))))

  `(test-with string-split
     "Test our own, non-crazy, string splitting"
     ,generator
     (lambda (str sep bits)
       (should (equal (string-split str sep) bits)))))

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
