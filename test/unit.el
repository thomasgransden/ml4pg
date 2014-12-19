;; Unit tests for ML4PG. Uses Emacs Lisp Regression Testing (ERT)

;; We only focus on plain Coq for now. Feel free to add SSReflect tests!
(ml4pg-load-coq)

;; ERT is quite basic, so we build a mini framework on top

(defconst test-iterations 25)

;; TODO: Print args whenever tests fails, since they might be random
(defmacro test-with (name doc generator tests)
  "Declare a test which uses a data generator.
     name is the test name, which will be prefixed by ml4pg-
     doc is a mandatory docstring
     generator is a zero-argument function which produces a list of test data
     tests is a function containing tests (eg. 'should' statements).
   The arguments of tests will be taken from the list returned by generator."
  (let ((namesym (intern (concat "ml4pg-" (symbol-name name)))))
    `(ert-deftest ,namesym ()
       ,doc
       (dotimes (iteration test-iterations)
         (let ((args (funcall ,generator)))
           (apply ,tests args))))))

(let ((accumulator nil))
  (defun accumulate (n)
    (setq accumulator (cons n accumulator))))

(ert-deftest ml4pg-test-with-works ()
  "Test our testing macro to make sure it actually defines tests,
   that successful tests pass and failing tests fail, and that
   generators are executed."
  (flet ((run-test ()
                   (ert-run-test (ert-get-test 'ml4pg-macro-test))))

    ;; Delete any existing macro test
    (ert-delete-test 'ml4pg-macro-test)
    (should-not (ert-test-boundp 'ml4pg-macro-test))

    ;; Try to define a new macro test
    (test-with macro-test
               "Testing the test-with macro"
               (lambda () '(t))
               (lambda (x) (should x)))
    (should (ert-test-boundp 'ml4pg-macro-test))

    ;; The test should pass
    (should (ert-test-passed-p (run-test)))

    ;; Do the same for a failing test, since they're more important
    (ert-delete-test 'ml4pg-macro-test)
    (test-with macro-test
               "Testing a failing test"
               (lambda () '(nil))
               (lambda (x) (should x)))
    (let ((ert-debug-on-error nil))
      (should (ert-test-failed-p (run-test))))

    ;; Make sure our generators and tests are running enough times
    (ert-delete-test 'ml4pg-macro-test)
    (test-with macro-test
               "Testing iteration"
               (lambda () (list iteration))
               (lambda (gen-it)
                 (accumulate (cons gen-it iteration))))
    (let ((accumulator nil))
      (run-test)
      (should (equal (length accumulator) test-iterations))
       (dotimes (i test-iterations)
         (should (member (cons i i) accumulator))))

    ;; Clean up
    (ert-delete-test 'ml4pg-macro-test)
    (should-not (ert-test-boundp 'ml4pg-macro-test))))

;; Generators

(defmacro lone (f)
  `(lambda ()
     (list (funcall ,f))))

(defun gen-bool ()
  "Generate t or nil"
  (equal (random 2) 0))

(defun gen-char ()
  "Generate a random ASCII character"
  (format "%c" (random 255)))

(defun gen-string (&optional op-len)
  "Generate a random ASCII string, of given (or random) length"
  (let ((len (or op-len (random 255))))
    (if (<= len 0)
        ""
        (concat (gen-char)
                (gen-string (1- len))))))

(defun gen-list (elem-gen &optional op-len)
  "Generate a random list, using the given element-generating function, of the
   given (or random) length"
  (let ((len (or op-len (random 255))))
    (if (<= len 0)
        nil
      (cons (funcall elem-gen) (gen-list elem-gen (1- len))))))

;; Test generators

(test-with gen-char
           "Character generator"
           (lone 'gen-char)
           (lambda (c)
             (should (equal 1 (length c)))
             (should (stringp c))))

(test-with gen-string
           "String generator"
           (lambda () (list (gen-string) (random 255)))
           (lambda (s n)
             (should (stringp s))
             (should (equal (format "%s" s) s))
             (should (equal (length (gen-string n)) n))))

(test-with gen-list
           "List generator"
           (lambda ()
             (list (gen-list (lambda () nil))
                   (random 255)))
           (lambda (lst n)
             (should (listp lst))
             (should (equal (length (gen-list 'gen-bool n)) n))))

;; Test string helper functions

(test-with between-spaces
           "Test between-spaces, for extracting Coq names"
           (lambda () (gen-list 'gen-string (+ 3 (random 255))))
           (lambda (&rest in-strs)
             (let* ((str  (mapconcat 'identity in-strs " "))
                    (strs (split-string str " ")))
               (should (equal (between-spaces "foo bar baz") "bar"))
               (should (equal (between-spaces str) (nth 1 strs))))))

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

(ert-deftest ml4pg-lookup-types ()
  "Looks up types in an assoc list"
  (should (equal (lookup-type-id '(("foo" . 10) ("bar" . 20))
                                 "foo")
                 10)))

(ert-deftest ml4pg-get-type-id ()
  "Extracts types from a string and looks them up in an assoc list"
  (should (equal (get-type-id-aux '(("foo" . 10) ("bar" . 20))
                                  "baz : foo")
                 10)))

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

(ert-deftest ml4pg-str-between ()
  (should (equal (str-between "abcdefghijkl" "c" "ijk") "defgh")))
