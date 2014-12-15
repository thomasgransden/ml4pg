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

(ert-deftest ml4pg-find-dot ()
  (should (equal (find-dot "abc.") 3)))

(ert-deftest ml4pg-pos-to-dot ()
  (should (equal (pos-to-dot "abc.def.ghi" 1) "bc")))

(ert-deftest ml4pg-take-30 ()
  "Should extract 30 items from a list"
  (should (equal (take-30 (generate-zeros 40))
                 (generate-zeros 30))))

(ert-deftest ml4pg-find-max-length ()
  "Finds the length of the longest saved theorem"
  (should (equal (find-max-length-aux '("foo" "bizzle" "boop")) 6)))

;; The meaty functions

(ert-deftest ml4pg-extract-theorem-id ()
  "Test theorem ID extraction"
  (should (equal (extract-theorem-id "foo bar <- baz.") 123)))
