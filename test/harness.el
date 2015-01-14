;; ERT is quite basic, so we build a mini framework on top

(defconst test-iterations 10)

(setq max-lisp-eval-depth 10000) ;; Hacky, but works for now

;; TODO: Print args whenever tests fails, since they might be random
(defmacro test-with (name doc generator tests)
  "Declare a test which uses a data generator:

     name is the test name, which will be prefixed by 'ml4pg-'
     doc is a mandatory docstring
     generator is a zero-argument function which produces a list of args
     tests is the test function; it should accept args from generator"
  (let ((namesym (intern (concat "ml4pg-" (symbol-name name)))))
    `(ert-deftest ,namesym ()
       ,doc
       (dotimes (iteration test-iterations)
         (let ((args (funcall ,generator)))
           (apply ,tests args))))))

(let ((ml4pg-test-accumulator nil))
  (defun ml4pg-test-accumulate (n)
    "Only used for testing purposes"
    (setq ml4pg-test-accumulator (cons n ml4pg-test-accumulator))))

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
