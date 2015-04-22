;; Generic, pure functions for common, everyday tasks

(defconst nl "
")

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

(defun replace-nth (list index elem)
  "Return a copy of LIST, with element INDEX replaced with ELEM"
  (let ((new nil))
    (dotimes (n (length list) (reverse new))
      (setq new (cons (if (= n index)
                          elem
                        (nth n list))
                      new)))))

(defun compose (&rest funcs)
  (unless funcs (error "Nothing to compose"))
  `(lambda (&rest args)
     (let ((result args))
       (dolist (func (reverse ',funcs) (car result))
         (setq result (list (apply func result)))))))

(defun f-and (&rest args)
  "Boolean AND, implemented as a proper function"
  (let ((result t))
    (dolist (elem args result)
      (setf result (and result elem)))))

(defun f-or (&rest args)
  "Boolean OR, implemented as a proper function"
  (let ((result nil))
    (dolist (elem args result)
      (setf result (or result elem)))))

(defun any (lst)
  "Non-nil iff any element of LST is non-nil"
  (apply 'f-or lst))

(defun all (lst)
  "Nil iff any element of LST is nil"
  (apply 'f-and lst))

(defun any-which (lst f &rest args)
  "Apply F to all elements of LST and see if any returned non-nil.
   Optionally, additional ARGS will be passed to F as initial args."
  (any (mapcar (apply 'apply-partially (cons f args)) lst)))

(defun tree-of-numbers (x)
  "Returns nil if X is not a number or a list of (list of...) numbers"
  (or (numberp x)
      (and (listp x) (all (mapcar 'tree-of-numbers x)))))

(defun remove-alone (list)
  (let (result)
    (dolist (elem list result)
      (unless (= (length elem) 1)
        (setf result (append result (list elem)))))))

(defun remove-whitespaces (string)
  (replace-regexp-in-string "  " " " string))

(defun extract-coq-names-from (str)
  (mapcar (lambda (s)
            (replace-regexp-in-string (format "\\(%s\\)\\|[\s\n]+"
                                              coq-declaration-re)
                                      ""
                                      s))
          (extract-coq-names-from-aux str)))

(defconst coq-declaration-re
  (join-strings '("Theorem" "Lemma" "Fact" "Remark" "Corollary" "Definition"
                  "Fixpoint" "CoFixpoint" "Example" "Proposition")
                "\\|")
  "A regexp matching Coq declaration keywords")

(defun extract-coq-names-from-aux (str)
  "An unreliable, conservative way to extract some of the names defined in a
   string of Coq vernacular. Note that this does *not* respect Sections and
   Modules, so the names you get back might not be defined globally."
  (let* ((ws "[\s\n]+")
         (re (format "\\(%s\\)%s[a-zA-Z0-9_]+%s"
                     coq-declaration-re ws ws)))
    (re-seq re str)))

(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun strip-parens (x)
  "Remove [, ], { and } chars from a string"
  (replace-regexp-in-string
   (regexp-opt (list "[" "]" "{" "}"))
   ""
   x))

(defun strip-spaces (x)
  "Remove space characters from a string"
  (replace-regexp-in-string " " "" x))

(defun strip-quotes (x)
  "Remove quote characters from a string"
  (replace-regexp-in-string (regexp-quote "\"") "" x))
