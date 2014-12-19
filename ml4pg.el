;; Package-Requires: ((dash "2.10.0") (dash-functional "1.2.0") (emacs "24"))

(require 'dash)
(require 'dash-functional)
(require 'shadchen)

;; FIXME: Don't hard-code paths
(defvar home-dir "/home/chris/Programming/ML4PG/")
(defconst *weka-dir* (concat home-dir "weka.jar"))
(defvar *matlab-program* nil)

(defvar mode nil)

;; FIXME: We should use lexical scope. I used quote splicing instead because
;;        I don't know if this whole system will fall over with lexical scope...
(defun load-els (dir)
  `(lambda (f)
     (load-file (concat home-dir ,dir "/" f ".el"))))

(defun ml4pg-load-coq ()
  (mapc (load-els "coq")
        '("auxiliary_files" "feature_extractionv2" "matlab_interaction"
          "shortcuts" "menus" "storage" "save_lemmas" "weka" "automaton"))
  (mapc (load-els "ssreflect")
        '("term-tree-definition" "term-tree-theorem" "extraction"
          "table-to-feature-vector" "weka-definitions" "storage-defs" "diagraph"
          "clusterdigraph" "trees")))

(defun ml4pg-load-ss ()
  (mapc (load-els "ssreflect")
        '("auxiliary_files" "feature_extraction_2" "matlab_interaction"
          "shortcuts" "menus" "storage" "save_lemmas" "weka"
          "term-tree-definition" "term-tree-theorem" "extraction"
          "table-to-feature-vector" "weka-definitions" "storage-defs"
          "automaton" "diagraph" "clusterdigraph" "trees")))

(defun select-mode ()
  (interactive)
  (let ((smode (read-string "What mode do you want to use (Coq -> c (default), SSReflect -> s, None -> n) : ")))
    (setq mode smode)
    (cond ((string= mode "s") (ml4pg-load-ss))
          ((string= mode "n") nil)
          (t                  (ml4pg-load-coq)))))

(require 'cl)

;; FIXME: Why on Earth are we deleting other windows and navigating to previous
;;        buffers? It just screws up Emacs; we should stop it.
(add-to-list 'auto-mode-alist
             '("\\.v\\'" . (lambda ()
                             (progn (coq-mode)
                                    (select-mode)
                                    (delete-other-windows)
                                    (previous-buffer)
                                    (previous-buffer)))))
