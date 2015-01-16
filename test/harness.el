;; ERT is quite basic, so we build a mini framework on top

(defconst test-iterations 8
  "Repeat tests this many times, to exercise our data generators a little")

(defvar ml4pg-test-complexity 1
  "Sets the complexity of generated test data. Increases as tests iterate.")

(setq max-lisp-eval-depth 10000) ;; Hacky, but works for now

(defmacro test-with (name doc rawgen test)
  "Declare an ML4PG test. Tests should try to be as reproducible as possible,
   to ease debugging. To get different behaviour on different runs, for example
   using randomly-generated data, you should split your test into a reproducible
   part (the test) which accepts arguments, and an unreproducible part (the
   generator) which returns a list of values for these arguments.

     'name' is the test name, which will be prefixed by 'ml4pg-'
     'doc' is a mandatory docstring
     'generator' is either a zero-argument function returning a list of values
     to be used as arguments for 'test', or nil if 'test' accepts no arguments
     'test' is the test function. If 'generator' is nil it should accept no
     arguments; otherwise it should accept one argument for each element in the
     return value of 'generator'"
  (let* ((namestr   (symbol-name name))
         (namesym   (intern (concat "ml4pg-" namestr)))
         (generator (eval rawgen))
         (recgen    (when generator (compose 'ml4pg-record-args generator))))
    `(ert-deftest ,namesym ()
       `doc
       (setq ml4pg-test-complexity  1)
       (setq ml4pg-check-complexity 0)
       (ml4pg-run-a-test ,namestr
                         (compose 'ml4pg-increase-complexity ,test)
                         ,recgen))))

(defun ml4pg-increase-complexity (&rest args)
  (setq ml4pg-test-complexity (* 2 ml4pg-test-complexity)))

(defun ml4pg-record-args (args)
  "Show a test's arguments in the *Messages* buffer, without echoing."
  (save-excursion
    (set-buffer "*Messages*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert (format "Arguments:\n%s\n"
                      (join-strings (mapcar 'any-to-string args) "\n")))))
  args)

(defun ml4pg-run-a-test (name test &optional generator)
  "Runs a TEST with the given NAME on the (optional) ARGS.
   This provides a handy way to instrument all tests."
  (dotimes (iteration (if generator test-iterations 1))
    (let ((args (when generator (funcall generator))))
      (apply test args))))

(defun ml4pg-run-test ()
  "DO NOT USE: This is for testing our test macro. Don't use in your own code"
  (ert-run-test (ert-get-test 'ml4pg-macro-test)))

(defun ml4pg-load-and-extract-info (str action)
  "Insert 'str' into a temporary buffer, load ML4PG in that buffer then run
   'action'"
  (with-temp-buffer
    (let ((ml4pg-interactive nil))
      (insert str)
      (ml4pg-mode)
      (coq-build-prog-args)
      (goto-char (point-max))
      (extract-feature-theorems)
      (funcall action))))
