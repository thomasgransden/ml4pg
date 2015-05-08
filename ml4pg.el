(unless (getenv "ML4PG_HOME")
  (error "ML4PG_HOME environment variable must be set"))

(defconst home-dir (let ((raw (getenv "ML4PG_HOME")))
                     (if (string= "/" (substring raw (1- (length raw))))
                         raw
                       (concat raw "/"))))

(defconst *weka-dir* (concat home-dir "weka.jar"))
(defvar *matlab-program* nil)

(defvar mode nil)

(defun select-mode ()
  (interactive)
  (let ((smode (read-string "What mode do you want to use (Coq -> c (default), SSReflect -> s, None -> n): ")))
    (setq mode smode)
    (cond ((string= mode "s") 
	   (progn  (load-file (concat home-dir "ssreflect/auxiliary_files.el"))
		   (load-file (concat home-dir "ssreflect/feature_extraction_2.el"))
		   (load-file (concat home-dir "ssreflect/matlab_interaction.el"))
		   (load-file (concat home-dir "ssreflect/shortcuts.el"))
		   (load-file (concat home-dir "ssreflect/menus.el"))
		   (load-file (concat home-dir "ssreflect/storage.el"))
		   (load-file (concat home-dir "ssreflect/save_lemmas.el"))
		   (load-file (concat home-dir "ssreflect/weka.el"))
		   (load-file (concat home-dir "ssreflect/term-tree-definition.el"))
		   (load-file (concat home-dir "ssreflect/term-tree-theorem.el"))
		   (load-file (concat home-dir "ssreflect/extraction.el"))
		   (load-file (concat home-dir "ssreflect/table-to-feature-vector.el"))
		   (load-file (concat home-dir "ssreflect/weka-definitions.el"))
		   (load-file (concat home-dir "ssreflect/storage-defs.el"))
		   (load-file (concat home-dir "ssreflect/automaton.el"))
		   (load-file (concat home-dir "ssreflect/diagraph.el"))
		   (load-file (concat home-dir "ssreflect/clusterdigraph.el"))
		   (load-file (concat home-dir "ssreflect/trees.el"))
		  ;; (init-clusters)
		   
		   ))
	  ((string= mode "n") nil)
	  (t (progn (load-file (concat home-dir "coq/auxiliary_files.el"))
		   (load-file (concat home-dir "coq/feature_extractionv2.el"))
		   (load-file (concat home-dir "coq/matlab_interaction.el"))
		   (load-file (concat home-dir "coq/shortcuts.el"))
		   (load-file (concat home-dir "coq/menus.el"))
		   (load-file (concat home-dir "coq/storage.el"))
		   (load-file (concat home-dir "coq/save_lemmas.el"))
		   (load-file (concat home-dir "coq/weka.el"))  
		   (load-file (concat home-dir "ssreflect/term-tree-definition.el"))
		   (load-file (concat home-dir "ssreflect/term-tree-theorem.el"))
		   (load-file (concat home-dir "ssreflect/extraction.el"))
		   (load-file (concat home-dir "ssreflect/table-to-feature-vector.el"))
		   (load-file (concat home-dir "ssreflect/weka-definitions.el"))
		   (load-file (concat home-dir "ssreflect/storage-defs.el"))
		   (load-file (concat home-dir "coq/automaton.el"))
		   (load-file (concat home-dir "ssreflect/diagraph.el"))
		   (load-file (concat home-dir "ssreflect/clusterdigraph.el"))
		   (load-file (concat home-dir "ssreflect/trees.el"))
		 ;;  (load-tactics)
		  ;; (init-clusters)
	))
	  )))

(require 'cl)

(add-to-list 'auto-mode-alist
             '("\\.v\\'" . (lambda ()
			     (progn (coq-mode) (select-mode) (delete-other-windows) (previous-buffer) (previous-buffer))
                               )))






