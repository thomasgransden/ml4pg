;; Unit tests for ML4PG. Uses Emacs Lisp Regression Testing (ERT)

;; We only focus on plain Coq for now. Feel free to add SSReflect tests!
(ml4pg-load-coq)

;; Helper functions

(ert-deftest ml4pg-between-spaces ()
  "Test between-spaces, for extracting Coq names"
  (should (equal (between-spaces "foo bar baz") "bar")))

(ert-deftest ml4pg-after-space ()
  "Finding the position of the text after a single space"
  (should (equal (after-space "foo bar") 4)))

(ert-deftest ml4pg-first-dot ()
  (should (equal (first-dot "abc.") 3)))

(ert-deftest ml4pg-pos-to-dot ()
  (should (equal (pos-to-dot "abc.def.ghi" 1) "bc")))

(ert-deftest ml4pg-take-30 ()
  "Should extract 30 items from a list"
  (should (equal (take-30 (generate-zeros 40))
                 (generate-zeros 30))))

(ert-deftest ml4pg-find-max-length ()
  "Finds the length of the longest saved theorem"
  (should (equal (find-max-length '("foo" "bizzle" "boop")) 6)))

;; The meaty functions

(ert-deftest ml4pg-append-to ()
  "Check we can append to variables"
  (let ((abcdefg '(1 2 3)))
    (append-to abcdefg 4)
    (should (equal abcdefg '(1 2 3 4)))))

(ert-deftest ml4pg-append-hyp ()
  "Check we can append to the hypothesis"
  (let ((hypothesis (list 10)))
    (append-hyp (list 20))
    (should (equal hypothesis (list 10 20)))))

(ert-deftest ml4pg-first-space ()
  "Check we can find the first space in a string"
  (should (equal (first-space "abc def") 3)))
