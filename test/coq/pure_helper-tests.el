(test-with flatten
  "Turn values into 1D lists"
  nil
  (lambda ()
    (should (equal (flatten nil) nil))
    (should (equal (flatten 10) '(10)))
    (should (equal (flatten '(1 2 3)) '(1 2 3)))
    (should (equal (flatten '((1 2) (((3) 4 (5 (6 7))) 8) 9))
                   '(1 2 3 4 5 6 7 8 9)))))

(test-with between-spaces
  "Test between-spaces, for extracting Coq names"
  (gen-list (gen-nonempty-string) (lambda () (+ 3 (funcall (gen-num)))))
  (lambda (&rest in-strs)
    (let* ((str  (mapconcat 'identity in-strs " "))
           (strs (string-split str " ")))
      (should (equal (between-spaces "foo bar baz") "bar"))
      (should (equal (between-spaces str) (nth 1 strs))))))

(test-with first-dot
  "Test looking for '.' in strings"
  (list-of (gen-string-without ".")
           (gen-string))
  (lambda (start end)
    (should (equal (first-dot (concat start "." end))
                   (length start)))))

(test-with pos-to-dot
  "Test chopping up to dots"
  (lambda ()
    (let ((prefix (funcall (gen-nonempty-string)))
          (middle (funcall (gen-string-without ".")))
          (suffix (funcall (gen-nonempty-string))))
      (list (concat prefix middle "." suffix) middle (length prefix))))
  (lambda (str middle n)
    (should (equal (pos-to-dot str n) middle))))

(test-with first-space
  "Check we can find the first space in a string"
  (list-of (gen-string-without " ")
           (gen-nonempty-string))
  (lambda (s1 s2)
    (should (equal (first-space (concat s1 " " s2)) (length s1)))))

(test-with str-between
  "Test extracting strings"
  (lambda ()
    (let* ((start  (funcall (gen-nonempty-string)))
           (end    (funcall (gen-nonempty-string)))
           (prefix (funcall (gen-string-without start)))
           (middle (funcall (gen-string-without end)))
           (suffix (funcall (gen-nonempty-string))))
      (list start end middle (concat prefix start middle end suffix))))
  (lambda (start end desired str)
    (should (equal (str-between str start end) desired))))

(test-with str-up-to
  "Test extracting prefixes of strings"
  (lambda ()
    (let* ((end    (funcall (gen-nonempty-string)))
           (prefix (funcall (gen-string-without end))))
      (list (concat prefix end (funcall (gen-string))) end prefix)))
  (lambda (str end prefix)
    (should (equal (str-up-to str end) prefix))))

;; These tests are pretty similar, so they share a generator
(let ((generator (lambda ()
                   (let* ((sep      (funcall (gen-any (gen-nonempty-string)
                                                      (gen-char))))
                          (bits     (gen-list (gen-string-without sep))))
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
  (list-of (gen-string-without " ") (gen-nonempty-string))
  (lambda (start end)
    (should (equal (after-space (concat start " " end))
                   (1+ (length start))))))

(test-with take-n
  "Should extract N items from a list"
  (list-of (gen-list (gen-string)) (gen-num))
  (lambda (l n)
    (let ((result (take-n n l)))
      (should (<= (length result) n))
      (should (<= (length result) (length l)))
      (when (>= (length l) n)
        (should (equal (length result) n))))))

(test-with take-30
  "Should extract 30 items from a list"
  (list-of (gen-list (gen-string) (lambda () (+ 30 (funcall (gen-num))))))
  (lambda (l)
    (should (equal (length (take-30 l)) 30))
    (dotimes (n 30)
      (should (equal (nth n (take-30 l))
                     (nth n l))))))

(test-with drop-n
  "Should remove the first N elements of a list"
  (list-of (gen-list (gen-string)) (gen-num))
  (lambda (lst n)
    (let ((result (drop-n n lst)))
      (should (<= (length result) (length lst)))
      (when (>= (length lst) n)
        (should (equal (length result) (- (length lst) n)))))))

(test-with find-max-length
  "Finds the length of the longest saved theorem"
  (lambda ()
    (let ((n (+ ml4pg-test-complexity (funcall (gen-num)))))
      (list (funcall (gen-list (gen-string)))
            (funcall (gen-string n))
            n)))
  (lambda (thms thm n)
    (should (equal (length thm) n))
    (should (equal (find-max-length (cons thm thms)) n))))

(test-with replace-nth
  "Test substituting elements in lists"
  (lambda ()
    (let* ((prefix  (funcall (gen-list (gen-num))))
           (suffix  (funcall (gen-list (gen-num))))
           (pre     (funcall (gen-num)))
           (post    (funcall (gen-num))))
      (list (append prefix (list pre) suffix)
            (length prefix)
            post
            (append prefix (list post) suffix))))
  (lambda (before n elem after)
    (should (equal (replace-nth before n elem)
                   after))))

(test-with any-which-none
  "Check if any element of a list passes a test"
  (compose (lambda (args) (list (funcall (list-of (car args)))
                                (cadr args)))
           (gen-elem (list (list (gen-string) 'stringp)
                           (list (gen-bool)   'booleanp)
                           (list (gen-num)    'numberp))))
  (lambda (lst f)
    (should (not (any-which lst (compose 'not f))))))

(test-with remove-whitespaces
  "Can remove redundant whitespace"
  (list-of (gen-string-without " ")
           (gen-string-without " "))
  (lambda (pre post)
    (should (equal (concat pre " " post)
                   (remove-whitespaces (concat pre "  " post))))))

(test-with remove-whitepace
  "Can remove all whitespace"
  (list-of (gen-string))
  (lambda (str)
    (let ((result (remove-whitespace str)))
      (should-not (search " "  result))
      (should-not (search "\n" result))
      (should-not (search nl   result))
      (should-not (search "\t" result))
      (should-not (search "\r" result)))))

(test-with string/reverse-ends
  "String reversal swaps the start and end"
  (list-of (gen-nonempty-string))
  (lambda (str)
    (should (equal (subseq (string/reverse str) 0 (1- (length str)))
                   (string/reverse (subseq str 1))))))

(test-with string/reverse-length
  "String reversal preserves length"
  (list-of (gen-string))
  (lambda (str)
    (should (equal (length str) (length (string/reverse str))))))

(test-with string/reverse-inverse
  "String reversal is its own inverse"
  (list-of (gen-string))
  (lambda (str)
    (should (equal str (string/reverse (string/reverse str))))))

(test-with match-at-end-reflexive
  "Strings match themselves"
  (list-of (gen-string))
  (lambda (str)
    (should (match-at-end str str))))

(test-with match-at-end-concat
  "Concat yields a match"
  (list-of (gen-string) (gen-string))
  (lambda (str1 str2)
    (should (match-at-end (concat str1 str2) str2))))

(test-with match-at-end-too-short
  "Can't get a match if it doesn't fit"
  (compose (lambda (strs)
             (let* ((str1 (car  strs))
                    (str2 (cadr strs))
                    ;; Ensure one string is shorter than the other
                    (trimmed (if (= (length str1) (length str2))
                                 (subseq str2 1)
                               str2)))
               ;; Return the longest then the shortest
               (if (> (length str1) (length trimmed))
                   (list str1    trimmed)
                   (list trimmed str1))))
           (list-of (gen-nonempty-string) (gen-nonempty-string)))
  (lambda (big small)
    (should (> (length big) (length small)))
    (should-not (match-at-end small big))))

(test-with strip-trailing
  "Test stripping trailing strings"
  (list-of (gen-string) (gen-list (gen-nonempty-string)))
  (lambda (str strs)
    (let* ((result (apply 'strip-trailing (cons str strs)))
           (lenr   (length result)))
      (dolist (end strs)
        (let ((lene (length end)))
          (if (> lenr lene)
              (should-not (equal end (subseq result (- lenr lene))))
              (should-not (equal end result))))))))

(test-with can-strip-control-chars
  "Strip control characters from strings"
  (list-of (gen-string))
  (lambda (str)
    (dolist (char control-chars)
      (should-not (search (string char) (strip-control-chars str))))))

(test-with stripping-controls-leaves-rest
  "Stripping control characters leaves the rest intact"
  (list-of (gen-string))
  (lambda (str)
    (dolist (char (string-to-list str))
      (unless (member char control-chars)
        (should (search (string char)
                        (strip-control-chars str)))))))

(test-with between-spaces-equiv
  "Test whether a particular use of search is equivalent to between-spaces"
  (list-of (gen-string) (gen-string) (gen-string))
  (lambda (str1 str2 str3)
    (let* ((str     (concat str1 " " str2 " " str3))
           (between (between-spaces str))
           (bit1    (search " " str))
           (bit2    (1+ (or bit1 0)))
           (bit3    (search " " str :start2 bit2))
           (substr  (subseq str bit2 bit3)))
      (should (equal substr between)))))
