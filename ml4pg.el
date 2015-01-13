;; Package-Requires: ((emacs "24"))

(defconst home-dir (or (getenv "ML4PG_HOME") "/put/path/to/ML4PG/here"))
(defconst *weka-dir* (concat home-dir "weka.jar"))
(defvar *matlab-program* nil)

(defvar mode nil)

;; FIXME: We should use lexical scope. I used quote splicing instead because
;;        I don't know if this whole system will fall over with lexical scope...
(defun load-els (dir)
  `(lambda (f)
     (load-file (concat home-dir ,dir "/" f ".el"))))

(defun ml4pg-load-coq ()
  (mapc (load-els "generic")
        '("pure_helpers"))
  (mapc (load-els "coq")
        '("auxiliary_files" "feature_extraction_pure"
          "feature_extraction_commands" "feature_extractionv2"
          "matlab_interaction" "shortcuts" "menus" "storage" "save_lemmas"
          "weka" "automaton"))
  (mapc (load-els "ssreflect")
        '("term-tree-definition" "term-tree-theorem" "extraction"
          "table-to-feature-vector" "weka-definitions" "storage-defs" "diagraph"
          "clusterdigraph" "trees")))

(defun ml4pg-load-ss ()
  (mapc (loads-els "generic")
        '("pure_helpers"))
  (mapc (load-els "ssreflect")
        '("auxiliary_files" "feature_extraction_2" "matlab_interaction"
          "shortcuts" "menus" "storage" "save_lemmas" "weka"
          "term-tree-definition" "term-tree-theorem" "extraction"
          "table-to-feature-vector" "weka-definitions" "storage-defs"
          "automaton" "diagraph" "clusterdigraph" "trees")))

(defun select-mode ()
  (interactive)
  (let* ((msg   "What mode do you want to use (Coq -> c (default), SSReflect -> s, None -> n) : ")
         (smode (if noninteractive "" (read-string msg))))
    (setq mode smode)
    (cond ((string= mode "s") (ml4pg-load-ss))
          ((string= mode "n") nil)
          (t                  (ml4pg-load-coq)))))

(require 'cl)

(defun ml4pg-mode ()
  (coq-mode)
  (when noninteractive
    (coq-build-prog-args)  ;; PG assumes coqtop will never run non-interactively
    (setq proof-shell-fiddle-frames nil))  ;; Don't alter non-existent windows
  (select-mode))

(add-to-list 'auto-mode-alist
             '("\\.v\\'" . 'ml4pg-mode))
