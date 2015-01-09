;; Sending commands to Coq

(defun do-unset-printing ()
  (proof-shell-invisible-cmd-get-result (format "Unset Printing All")))

(defun do-check-object (object)
  (proof-shell-invisible-cmd-get-result (concat "Check " object)))

(defun do-set-printing ()
  (proof-shell-invisible-cmd-get-result (format "Set Printing All")))

(defun do-focus-unsafe ()
  (proof-shell-invisible-cmd-get-result "Focus"))

(defun do-focus ()
  (let ((result (do-focus-unsafe)))
    (if (search "Error:" result)
        (error "Problem during focus: %s" result)
        result)))

(defun do-show-intro ()
  (proof-shell-invisible-cmd-get-result "Show Intro"))

(defun do-intro ()
  (proof-shell-invisible-cmd-get-result "intro"))

(defun do-intro-of (name)
  (proof-shell-invisible-cmd-get-result (concat "intro " name)))

(defun do-show-intros ()
  (proof-shell-invisible-cmd-get-result (format "Show Intros")))

(defun do-undo ()
  (proof-shell-invisible-cmd-get-result (format "Undo")))

(defun do-induction-on (name)
  (proof-shell-invisible-cmd-get-result (concat "induction " name)))

(defun do-show-proof ()
  (proof-shell-invisible-cmd-get-result "Show Proof"))

(defun do-goal-str ()
  (do-set-printing)
  (goal-str-aux (do-focus)))
