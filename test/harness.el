;; ERT is quite basic, so we build a mini framework on top

;; Repeat tests this many times, to excercise our data generators a little
(defconst test-iterations 10)

(setq max-lisp-eval-depth 10000) ;; Hacky, but works for now

(defmacro test-with (name doc generator test)
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
  (let ((namesym (intern (concat "ml4pg-" (symbol-name name)))))
    (cond
     ((not generator)
      `(ert-deftest ,namesym ()
         ,doc
         (funcall ,test)))

     ((functionp generator)
      `(ert-deftest ,namesym ()
         ,doc
         (dotimes (iteration test-iterations)
           (let ((ml4pg-test-args (funcall ,generator)))
             (save-excursion
               (set-buffer "*Messages*")
               (goto-char (point-max))
               (let ((buffer-read-only nil))
                 (insert (format "Arguments:\n%s\n"
                                 (join-strings (mapcar 'any-to-string
                                                       ml4pg-test-args)
                                               "\n")))))
             (apply ,test ml4pg-test-args)))))

     ((listp generator))
     )))

(defun ml4pg-test-simplify (test args valid)
  "Takes a TEST and some ARGS which cause it to fail. We keep simplifying the
   ARGS until either the TEST passes, or the VALID predicate rejects them. We
   return the simplest, valid ARGS which cause TEST to fail."

  ((result (let ((ert-debug-on-error nil))
             89                     (ert-run-test test)))))

(defun measure-complexity (val)
  "A crude complexity measure for VAL, to help simplify test data"
  (cond
   ((numberp val)
    (floor (max 0 (log val 2))))

   ((stringp val)
    (length val))

   ((listp val)
    (apply '+ (mapcar 'measure-complexity val)))

   ((booleanp val)
    1)

   (t 0)))

(defun simpler (x y)
  "Check whether X is simpler than Y"
  (<= (measure-complexity x)
      (measure-complexity y)))

(defun ml4pg-simplify-data (val)
  (cond
   ((numberp val)
    (ml4pg-simplify-num val))

   ((listp val)
    (ml4pg-simplify-list val))

   ((stringp val)
    (ml4pg-simplify-string val))))

(defun ml4pg-simplify-num (n)
  "Simplified alternatives to N"
  (unless (or (= 0 n) (= 1 n))
    (list 1 0 (/ n 2))))

(defun random-elem (list)
  (when list (nth (random (length list)) list)))

(defun ml4pg-simplify-list (list)
  "Simplified alternatives to LIST"
  (if list
    (list nil                                  ;; Try using nil
          (subseq list 0 (/ (length list) 2))  ;; Chop off the end
          (subseq list   (/ (length list) 2))  ;; Chop off the start
          (mapcar (lambda (x)
                    (if (gen-bool)
                        (let ((alternatives (ml4pg-simplify-data x)))
                          (if alternatives
                              (random-elem alternatives)
                              x))
                        x))
                  list)))) ;; Simplify half of the content

(defun ml4pg-simplify-string (str)
  "Simplified alternatives to STR"
  (unless (string= str "")
    (list ""
          (subseq str 0 (/ (length str) 2))
          (subseq str   (/ (length str) 2)))))

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
