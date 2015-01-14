;; Generic, pure functions for common, everyday tasks

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure)
         (list structure))
        (t (mapcan #'flatten structure))))

(defun str-after (str pattern)
  (subseq str (+ (length pattern) (search pattern str))))

(defun between-spaces (txt)
  (let ((space (after-space txt)))
    (subseq txt space (search " " txt :start2 space))))

(defun first-dot (txt)
  "Find the position of the first dot in a string"
  (search "." txt))

(defun pos-to-dot (cmd n)
  "Extract a sub-string from the given position to the first dot"
  (subseq cmd n (first-dot cmd)))

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in))

(defun first-space (txt)
  "Find the position of the first space in a string"
  (search " " txt))

(defun after-space (txt)
  "Find the position of text after the first space"
  (1+ (first-space txt)))

(defun str-between (str start end)
  (let* ((bits (split-string str (regexp-quote start)))
         (suff (if (cdr bits)
                   (apply 'concat (cdr bits))
                 "")))
    (car (split-string suff (regexp-quote end)))))

(defun str-to (str end)
  (subseq str 0 end))

(defun take-30 (list)
  (take-n 30 list))

(defun take-n (n list)
  (if (= n 0)
      nil
    (cons (car list) (take-n (1- n) (cdr list)))))

(defun find-max-length (lst)
  (let ((result 0))
    (dolist (element lst result)
      (setq result (max (length element) result)))))

(defun createwebpage-aux (map)
  (format "<head>
             <title>Dependency Diagram</title>
           </head>
           <body>
             <img src='temp.png' usemap='#depend' />
             <map id='depend' name='depend'>%s</map>
           </body>" map))

(defun clusterofseveral-aux (txt)
  (concat "digraph {\n rankdir=LR;\n" txt "\n}"))
