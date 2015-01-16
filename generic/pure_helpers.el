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
  (let ((suffix (subseq cmd n)))
    (subseq suffix 0 (first-dot suffix))))

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in))

(defun first-space (txt)
  "Find the position of the first space in a string"
  (search " " txt))

(defun after-space (txt)
  "Find the position of text after the first space"
  (1+ (first-space txt)))

(defun str-between (str start end)
  (let* ((start-pos (length (str-up-to str start)))
         (suffix    (subseq str (+ (length start) start-pos))))
    (str-up-to suffix end)))

(defun str-up-to (str bit)
  (car (string-split str bit)))

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

(defun createwebpage (map b64)
  (format "<head>
             <title>Dependency Diagram</title>
           </head>
           <body>
             <img src='data:image/png;base64,%s' usemap='#depend' />
             <map id='depend' name='depend'>%s</map>
           </body>" b64 map))

(defun clusterofseveral-aux (txt)
  (concat "digraph {\n rankdir=LR;\n" txt "\n}"))

(defun join-strings (strs sep)
  "Concatenate STRS together, separated by SEP"
  (mapconcat 'identity strs sep))

(defun any-to-string (any)
  (format "%S" any))

(defun string-split (str sep)
  (let ((case-fold-search nil))
    (split-string str (regexp-quote sep))))

(defun nth-of (n tbl)
  (car (nth (1- n) tbl)))

(defun library-belong-aux (n list)
  (do ((defs list (cdr defs))
       (stop nil)
       (lib "")
       (acc 0))
      (stop lib)
    (if (< n (+ acc (cadr (car defs))))
        (progn (setf stop t)
               (setf lib (car (car defs))))
      (setf acc (+ acc (cadr (car defs)))))))

(defun clusterofone-node (elem tbl defs counter)
  (format "subgraph cluster%s {\n%s\n}\n"
          counter (clusterofone elem (1+ counter) tbl defs)))

(defun clusterofone-leaf (elem tbl defs next)
  (let ((first  (nth-of elem tbl))
        (second (library-belong-aux (1- elem) defs)))
    (if (listp next)
        (format "%s [URL=\"./%s.html\"];\n"
                first second)
      (format "%s [URL=\"./%s.html#%s\"]; %s -> %s[style=invis]\n"
              first second first first (nth-of next tbl)))))

(defun clusterofone (lst counter tbl defs)
  (do ((temp lst (cdr temp))
       (res ""))
      ((endp temp) res)
    (let ((elem (car temp)))
      (concat-to res (if (listp elem)
                         (clusterofone-node elem tbl defs counter)
                       (clusterofone-leaf elem tbl defs
                                          (when (cdr temp) (cadr temp))))))))

(defun replace-nth (list index elem)
  "Return a copy of LIST, with element INDEX replaced with ELEM"
  (let ((new nil))
    (dotimes (n (length list) (reverse new))
      (setq new (cons (if (= n index)
                          elem
                        (nth n list))
                      new)))))

(defun issubcluster (cluster1 cluster2)
  (let (missing)
    (dolist (elem cluster1 (not missing))
      (setq missing (or missing
                        (not (member elem cluster2)))))))

(defun compose (&rest funcs)
  (unless funcs (error "Nothing to compose"))
  `(lambda (&rest args)
     (let ((result args))
       (dolist (func (reverse ',funcs) (car result))
         (setq result (list (apply func result)))))))
