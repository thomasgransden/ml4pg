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
  "Record a test's arguments (ARGS), to help reproduce failures. Returns ARGS."
  (write-to-messages
   `(lambda ()
      (insert (format "Arguments:\n%s\n"
                      (join-strings (mapcar 'any-to-string ',args) "\n")))))
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

(defun ml4pg-load-and-execute (str action)
  "Insert STR into a temporary buffer, load ML4PG in that buffer then run
   ACTION"
  (let ((path (make-temp-file "ml4pg-test" nil ".v")))
    (with-temp-file path
      (insert str))
    (run-with-temp-coq-file path action)))

(defmacro ml4pg-load-and-extract-info (str action)
  "Insert STR into a temporary buffer, load ML4PG, extract features then run
   ACTION"
  `(ml4pg-load-and-execute ,str
                           (lambda ()
                             (goto-char (point-max))
                             (extract-feature-theorems)
                             (test-msg "LOADED!")
                             (funcall ,action))))

(defun run-with-temp-coq-file (path func)
  ;; ProofGeneral only allows one active file, so deactivate any existing file
  (when proof-script-buffer
    (proof-deactivate-scripting 'retract))
  (let ((buf            nil)
        (noninteractive t))
    (unwind-protect
        (progn (find-file path)
               (setq buf (current-buffer))
               ;; Run func
               (funcall func))
      ;; Cleanup
      (when buf (kill-buffer buf))
      (delete-file path)
      (proof-script-remove-all-spans-and-deactivate)
      (let ((coq-recoverable t))
        (ignore-errors (proof-shell-exit t))))))

(defun with-coq-example (f)
  "Make a copy of the ml4pg.v example file, open it, execute F, then delete the
   copy and kill Coq. Useful for testing with real(istic) Coq code."
  ;; Copy and load ml4pg.v
  (let ((path (make-temp-file "ml4pg" nil ".v")))
    (kill-buffer
     (with-temp-file path
       (insert-file-contents-literally (concat home-dir "ml4pg.v"))
       (current-buffer)))
    (run-with-temp-coq-file path f)))

(defun coq-example-names ()
  (unless example-names
    (message "Getting names from ml4pg.v")
    (with-coq-example (lambda ()
                        (setq example-names
                              (extract-coq-names-from (buffer-string))))))
  example-names)

(defconst example-names nil
  "The names used in ml4pg.v (access via coq-example-names)")

(defun get-file-contents (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun get-and-kill-display ()
  "Kill the '*display*' buffer, returning its contents"
  (let ((disp (get-buffer "*display*")))
    (assert disp)
    (let ((content (unwind-protect
                       (with-current-buffer disp
                         (buffer-substring-no-properties (point-min) (point-max)))
                     (kill-buffer disp))))
      (test-msg (format "DISPLAY:\n%s\n" content))
      content)))

(defun clean-ml4pg-dir ()
  "Delete temp files"
  (mapcar (lambda (path)
            (ignore-errors (delete-file path)))
          (list "temp.csv" "temp.gv" "temp.html" "temp.png" "out.arff"
                "out_bis.arff" "temp3.arff")))
