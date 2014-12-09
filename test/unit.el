;; Unit tests for ML4PG. Uses Emacs Lisp Regression Testing (ERT)

(ert-deftest string-manipulation ()
  "Test the various text-handling functions of ML4PG"
  (should (equal (between-spaces "foo bar baz") "bar")))
