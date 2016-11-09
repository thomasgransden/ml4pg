(test-with flatten
  "Turn values into 1D lists"
  nil
  (lambda ()
    (should (equal (flatten nil) nil))
    (should (equal (flatten 10) '(10)))
    (should (equal (flatten '(1 2 3)) '(1 2 3)))
    (should (equal (flatten '((1 2) (((3) 4 (5 (6 7))) 8) 9))
                   '(1 2 3 4 5 6 7 8 9)))))

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
      (should-not (search "\t" result))
      (should-not (search "\r" result)))))

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

(test-with bump-to-above
  "Test bumping up numbers works"
  nil
  (lambda ()
    (should (equal 5 (bump-to-above 1 0 '(1 2 3 4 7 8 9))))))
