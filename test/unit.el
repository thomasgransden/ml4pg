;; Unit tests for ML4PG. Uses Emacs Lisp Regression Testing (ERT)

;; We only focus on plain Coq for now. Feel free to add SSReflect tests!

;; ERT is quite basic, so we build a mini framework on top

(defconst test-iterations 10)
(setq max-lisp-eval-depth 10000) ;; Hacky, but works for now

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

(defun gen-num ()
  "Generate positive random numbers"
  (random 255))

(defun gen-char ()
  "Generate a random ASCII character"
  (format "%c" (random 255)))

(defun gen-string (&optional op-len)
  "Generate a random ASCII string, of given (or random) length"
  (let ((len (or op-len (random 255)))
        (str ""))
    (dotimes (i len str)
      (setq str (concat str (gen-char))))))

(defun gen-nonempty-string ()
  "Generate a random ASCII string of at least one char"
  (gen-string (1+ (random 254))))

(defun gen-list (elem-gen &optional op-len)
  "Generate a random list, using the given element-generating function, of the
   given (or random) length"
  (let ((len (or op-len (random 255)))
        (lst nil))
    (dotimes (i len lst)
      (setq lst (cons (funcall elem-gen) lst)))))

(defun gen-pair (first second)
  (cons (funcall first) (funcall second)))

(defun gen-types-id ()
  "Generator for types_id values"
  (gen-list (lambda () (gen-pair 'gen-string 'gen-num))))

(defun gen-filtered (elem-gen filter)
  "Filters a generator using a predicate"
  (let ((val (funcall elem-gen)))
    (while (not (funcall filter val))
      (setq val (funcall elem-gen)))))

;; Test generators

(test-with gen-num
           "Number generator"
           (lone 'gen-num)
           (lambda (n)
             (should (numberp n))
             (should (>= n 0))))

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

(test-with gen-pair
           "Test pair generation"
           (lambda ())
           (lambda ()
             (let ((sb (gen-pair 'gen-string 'gen-bool))
                   (bs (gen-pair 'gen-bool 'gen-string)))
               (should (stringp  (car sb)))
               (should (booleanp (cdr sb)))
               (should (booleanp (car bs)))
               (should (stringp  (cdr bs))))))

(test-with gen-types-id
           "Test types_id value generation"
           (lone 'gen-types-id)
           (lambda (alist)
             (should (listp alist))
             (dotimes (n (length alist))
               (let* ((type (car (nth n alist)))
                      (id   (cdr (assoc type alist))))
                 (should (equal (assoc type alist)
                                (cons type id)))))))

(test-with gen-filtered
           "Test generator filters"
           (lambda ())
           (lambda ()
             (let ((test-len (lambda (s) (> 100 (length s)))))
               (should (funcall test-len
                                (gen-filtered 'gen-string test-len))))))

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

(defun ml4pg-load-and-extract-info (str action)
  (with-temp-buffer
    (let ((ml4pg-interactive nil))
      (insert str)
      (ml4pg-mode)
      (coq-build-prog-args)
      (goto-char (point-max))
      (extract-feature-theorems)
      (funcall action))))

(test-with extract-defs-empty
           "Test extracting definitions from an empty buffer"
           (lambda ())
           (lambda ()
             (ml4pg-load-and-extract-info "" 'dependencygraph-defs)
             (should t)))

(setq debug-on-error t)

;; Load ML4PG if needed. Don't reload, since it unsets edebug instrumentation.
(unless (boundp 'home-dir)
  (load (concat (getenv "ML4PG_HOME") "ml4pg.el")))
(unless (boundp 'saved-theorems)
  (ml4pg-load-coq))

;; Run tests
(funcall (if noninteractive 'ert-run-tests-batch 'ert) "^ml4pg-")
