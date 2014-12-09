;; Unit tests for ML4PG. Uses Emacs Lisp Regression Testing (ERT)

;; We only focus on plain Coq for now. Feel free to add SSReflect tests!
(ml4pg-load-coq)

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

(ert-deftest ml4pg-extract-theorem-id ()
  "Test theorem ID extraction"
  (should (equal (extract-theorem-id "foo bar <- baz.") 123)))
