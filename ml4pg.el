;; Package-Requires: ((emacs "24"))

(unless (getenv "ML4PG_HOME")
  (error "ML4PG_HOME environment variable must be set"))

(defconst home-dir (let ((raw (getenv "ML4PG_HOME")))
                     (if (string= "/" (substring raw (1- (length raw))))
                         raw
                         (concat raw "/"))))

(defconst *weka-dir* (concat home-dir "weka.jar"))

(defvar mode nil)

(defun load-els (dir)
  `(lambda (f)
     (load-file (concat home-dir ,dir "/" f (if (search ".el" f) "" ".el")))))

(defun ml4pg-load-coq ()
  (mapc (load-els "generic")
        (directory-files (concat home-dir "generic/") nil ".*\.el"))
  (mapc (load-els "coq")
        '("auxiliary_files" "feature_extraction_pure"
          "feature_extraction_commands" "feature_extractionv2"
          "matlab_interaction" "shortcuts" "menus" "storage" "save_lemmas"
          "automaton"))
  (mapc (load-els "ssreflect")
        '("term-tree-definition" "term-tree-theorem" "extraction"
          "table-to-feature-vector" "weka-definitions" "storage-defs" "diagraph"
          "clusterdigraph" "graph_pure" "trees")))

(defun ml4pg-load-ss ()
  (mapc (loads-els "generic")
        '("pure_helpers" "impure_helpers"))
  (mapc (load-els "ssreflect")
        '("auxiliary_files" "feature_extraction_2" "matlab_interaction"
          "shortcuts" "menus" "storage" "save_lemmas"
          "term-tree-definition" "term-tree-theorem" "extraction"
          "table-to-feature-vector" "weka-definitions" "storage-defs"
          "automaton" "diagraph" "clusterdigraph" "graph_pure" "trees")))

(defun select-mode ()
  (interactive)
  (let* ((msg   "What mode do you want to use (Coq -> c (default), SSReflect -> s, None -> n) : ")
         (smode (if noninteractive "" (read-string msg))))
    (setq mode smode)
    (cond ((string= mode "s") (ml4pg-load-ss))
          ((string= mode "n") nil)
          (t                  (ml4pg-load-coq)))))

(require 'cl)

(defun ml4pg-mode-aux ()
  (when noninteractive
    (coq-build-prog-args)  ;; PG assumes coqtop will never run non-interactively
    (setq proof-shell-fiddle-frames nil)  ;; Don't alter non-existent windows
    (setq proof-three-window-enable nil)))

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
      (coq-mode))
    (message "Finished loading Proof General")))

(use-nix-if-present)
(load-proof-general)

(add-hook 'coq-mode-hook 'ml4pg-mode)
