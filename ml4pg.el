;; Package-Requires: ((emacs "24"))

(unless (getenv "ML4PG_HOME")
  (error "ML4PG_HOME environment variable must be set"))

(defconst home-dir (let ((raw (getenv "ML4PG_HOME")))
                     (if (string= "/" (substring raw (1- (length raw))))
                         raw
                         (concat raw "/"))))

(defconst *weka-dir* (concat home-dir "weka.jar"))

(defvar mode (getenv "ML4PG_TYPE"))

(defun load-els (dir)
  `(lambda (f)
     (load-file (concat home-dir ,dir "/" f (if (search ".el" f) "" ".el")))))

(defun ml4pg-load-coq ()
  (mapc (load-els "generic")
        (directory-files (concat home-dir "generic/") nil ".*\.el"))
  (mapc (load-els "coq")
        '("auxiliary_files" "feature_extraction_pure"
          "feature_extractionv2"
          "matlab_interaction" "shortcuts" "menus" "storage" "save_lemmas"
          "automaton"))
  (mapc (load-els "ssreflect")
        '("term-tree-theorem" "extraction"
          "table-to-feature-vector" "weka-definitions" "storage-defs" "diagraph"
          "clusterdigraph" "graph_pure" "trees")))

(defun ml4pg-load-ss ()
  (mapc (load-els "generic")
        (directory-files (concat home-dir "generic/") nil ".*\.el"))
  (mapc (load-els "ssreflect")
        '("auxiliary_files" "feature_extraction_2" "matlab_interaction"
          "shortcuts" "menus" "storage" "save_lemmas"
          "term-tree-theorem" "extraction"
          "table-to-feature-vector" "weka-definitions" "storage-defs"
          "automaton" "diagraph" "clusterdigraph" "graph_pure" "trees")))

(defun select-mode ()
  (interactive)
  (unless (or noninteractive mode)
    (setq mode (case (read-string "What mode do you want to use (Coq -> c (default), SSReflect -> s, None -> n) : ")
                 ("s" "ssreflect")
                 ("n" nil)
                 (t   "coq"))))
  (test-msg (format "Loading for '%s'" mode))
  (cond ((string= mode "ssreflect") (ml4pg-load-ss))
        ((string= mode "n")         nil)
        (t                          (ml4pg-load-coq))))

(require 'cl)

(defun ml4pg-mode-aux ()
  (when noninteractive
    ;; Hack Proof General for noninteractive use
    (coq-build-prog-args)  ;; PG assumes coqtop will never run non-interactively
    (setq proof-shell-fiddle-frames nil)  ;; Don't alter non-existent windows
    (setq proof-three-window-enable nil)
    (add-hook 'proof-shell-handle-error-or-interrupt-hook 'ml4pg-bail-out)))

(defconst coq-recoverable nil
  "If we've encountered a Coq error, are we in a position to handle it?")

(defun ml4pg-bail-out (&rest args)
  (unless coq-recoverable
    (when proof-shell-last-response-output
      (message "Last Coq response: %s" proof-shell-last-response-output))
    (message "COQ BUFFER\n%s\nEND COQ BUFFER" (coq-buffer-contents))
    (error "Caught error from Coq process. Details: %S" args)))

(defun ml4pg-mode ()
  (ml4pg-mode-aux)
  (select-mode))

(defun use-nix-if-present ()
  (dolist (path '("~/.nix-profile/share/emacs/site-lisp"
                  "/run/current-system/sw/share/emacs/site-lisp"))
    (when (and (file-exists-p path)
               (not (member path load-path)))
      (message "Adding %s to load path" path)
      (setq load-path (append load-path (list path))))))

(defun load-proof-general ()
  (unless (fboundp 'coq-build-prog-args)
    (message "Loading Proof General")
    (load "ProofGeneral/generic/proof-site")
    (with-temp-buffer
      (coq-mode))))

;; For debugging, especially reproducing test failures

(defun test-msg (s)
  "Writes a message, only displaying it when verbose"
  (write-to-messages `(lambda ()
                        (insert ,s))))

(defun write-to-messages (f)
  "Run F in the context of a writable *Messages* buffer"
  ;; FIXME: Make a LISP variable, which initialises to this env var, so we can
  ;; override it from LISP without altering the environment
  (if (equal "t" (getenv "TEST_VERBOSE"))
      ;; Regular message output, including to minibuffer/stdout
      (message (with-temp-buffer
                 (funcall f)
                 (replace-regexp-in-string "%" "%%" (buffer-string))))
    ;; Only write the *Messages*, not to minibuffer/stdout
    (save-excursion
      (set-buffer "*Messages*")
      (goto-char (point-max))
      (let ((buffer-read-only nil))
        (funcall f)))))

(add-hook 'coq-mode-hook 'ml4pg-mode)
(use-nix-if-present)
(load-proof-general)
