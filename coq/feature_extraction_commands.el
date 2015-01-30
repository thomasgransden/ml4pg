;; Sending commands to Coq

(defun do-unset-printing ()
  (send-coq-cmd "Unset Printing All"))

(defun do-check-object (object)
  (send-coq-cmd (concat "Check " object)))

(defun do-set-printing ()
  (send-coq-cmd "Set Printing All"))

(defun do-focus-unsafe ()
  (send-coq-cmd "Focus"))

(defun handle-error (msg)
  (message msg)
  (if noninteractive (progn (backtrace)
                            (kill-emacs))
                     (debug)))

(defun do-focus (&optional handler)
  (let ((result (do-focus-unsafe)))
    (if (search "Error:" result)
        (if handler (funcall handler result)
                    (handle-error (format "Problem during focus: %s" result)))
        result)))

(defun do-show-intro ()
  (send-coq-cmd "Show Intro"))

(defun do-intro ()
  (send-coq-cmd "intro"))

(defun do-intro-of (name)
  (send-coq-cmd (concat "intro " name)))

(defun do-show-intros ()
  (send-coq-cmd "Show Intros"))

(defun do-undo ()
  (send-coq-cmd "Undo"))

(defun do-induction-on (name)
  (send-coq-cmd (concat "induction " name)))

(defun do-show-proof ()
  (send-coq-cmd "Show Proof"))

(defun do-goal-str (&optional handler)
  (do-set-printing)
  (goal-str-aux (do-focus handler)))

(defun send-coq-cmd (str)
  (message "SENDING: %s" str)
  (let ((result (proof-shell-invisible-cmd-get-result str)))
    (message "GOT: %s" result)
    result))
