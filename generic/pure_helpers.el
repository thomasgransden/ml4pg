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
  (let* ((fst (after-space txt))
         (snd (search " " txt :start2 fst)))
    (unless fst (error "No spaces in '%s'" txt))
    (unless snd (error "Only one space in '%s'" txt))
    (subseq txt fst snd)))

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
  (if (or (= n 0) (null list))
      nil
      (cons (car list) (take-n (1- n) (cdr list)))))

(defun drop-n (n list)
  (if (or (= n 0) (null list))
      list
      (drop-n (1- n) (cdr list))))

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

(defun remove-whitespace (string)
  (strip-regexp string "[\s\n\r\t]"))

(defun extract-coq-names-from (str)
  (mapcar (lambda (s)
            (strip-regexp s  (format "\\(%s\\)\\|[\s\n]+" coq-declaration-re)))
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
  (strip-regexp x (regexp-opt (list "[" "]" "{" "}"))))

(defun strip-spaces (x)
  "Remove space characters from a string"
  (strip-str x " "))

(defun strip-quotes (x)
  "Remove quote characters from a string"
  (strip-str x "\""))

(defun subnum (big small)
  "Check whether the digits of SMALL appear in those of BIG"
  (let ((bigs   (number-to-string big))
        (smalls (number-to-string small)))
    (or (equal smalls "0")  ; Zeros can always be prepended
        (search "e" bigs)   ; Bail out on scientific notation
        (search "e" smalls) ; Ditto
        (search smalls bigs))))

(defun remove-squared-parenthesis (string res)
  (let* ((pos1 (search "[" string))
         (pos2 (search "{" string))
         (args (cond ((and pos1 pos2 (< pos1 pos2)) (list "]" pos1))
                     ((and pos1 pos2)               (list "}" pos2))
                     (pos1                          (list "]" pos1))
                     (pos2                          (list "}" pos2)))))
    (if args
        (remove-squared-parenthesis
         (subseq string (1+ (search (nth 0 args) string :start2 (nth 1 args))))
         (concat res (subseq string 0 (nth 1 args))))
      (concat res string))))

(defun remove-iterations (string)
  (strip-regexp string "[!?]"))

(defun extract-params2 (seq res)
  (extract-params-aux "." seq res))

(defun extract-params-aux (sep seq res)
  (let ((pos_space (first-space seq))
        (pos_jump  (search sep seq)))
    (if pos_space
        (extract-params-aux sep
                            (subseq seq (1+ pos_space))
                            (cons (subseq seq 0 pos_space) res))
      (reverse (cons (subseq seq 0 pos_jump) res)))))

(defun remove-empties (list)
  (message "FIXME: This is a filter")
  (let ((result nil))
    (dolist (elem list result)
      (unless (string= elem "")
        (append-to result elem)))))

(defun put-together-parenthesis (list)
  (message "FIXME: This parenthesis-handling looks dubious")
  (let ((n      0)
        (result nil)
        (aux    ""))
    (dolist (elem list result)
      (cond ((search "(" elem)
             (setf n (1+ n))
             (setf aux (concat aux elem " ")))

            ((and (search ")" elem)
                  (not (= 0 (- n (occurrences ")" elem)))))
             (setf n (- n (occurrences ")" elem)))
             (setf aux (concat aux elem " ")))

            ((search ")" elem)
             (setf n (1- n))
             (setf aux (concat aux elem))
             (append-to result aux)
             (setf aux ""))

            ((not (= n 0))
             (setf aux (concat aux elem " ")))

            (t
             (append-to result elem))))))

(defun extract-params3 (cmd)
  (put-together-parenthesis
   (remove-empties
    (extract-params2
     (remove-iterations
      (remove-squared-parenthesis cmd "")) nil))))

(defun extract-real-params (list)
  (message "FIXME: This is a filter")
  (let ((result nil))
    (dolist (elem list result)
      (unless (or (search  "->"  elem)
                  (search  "<-"  elem)
                  (string= "_"   elem)
                  (string= (subseq elem 0 1) "/"))
        (append-to result elem)))))

(defun string/reverse (str)
  "Reverse the str where str is a string"
  (if (equal str "")
      ""
      (apply 'string (reverse (string-to-list str)))))

(defun count-occurences (regex string)
  "Count how many times REGEX matches STRING"
  (recursive-count regex string 0))

(defun recursive-count (regex string start)
  "Helper for count-occurences"
  (if (string-match regex string start)
      (+ 1 (recursive-count regex string (match-end 0)))
      0))

(defun balanced-parens (s)
  "Check whether ( and ) are balanced. Doesn't check quotes or other bracket
   types ([, ], {, }, <, >, etc.)"
  (= (count-occurences (regexp-quote "(") s)
     (count-occurences (regexp-quote ")") s)))

(defun match-at-end (str end)
  "Does STR end with END?"
  (let ((lens (length str))
        (lene (length end)))
    (when (>= lens lene)
      (equal end (subseq str (- lens lene))))))

(defun strip-trailing (str &rest strs)
  "Drop characters from the end of STR until it doesn't end in any of STRS"
  (when (any (mapcar (lambda (s) (equal s "")) strs))
    (error "Can't strip off empty strings"))
  (do ((result str))
      ((not (any (mapcar (lambda (s) (match-at-end result s)) strs)))
       result)
    (dolist (end strs)
      (when (match-at-end result end)
        (setf result (subseq result 0 (- (length result) (length end))))))))

(defun strip-regexp (str &rest res)
  (let ((result str))
    (dolist (to-strip res result)
      (setq result (replace-regexp-in-string to-strip "" result)))))

(defun strip-str (str &rest strs)
  (apply 'strip-regexp (cons str (mapcar 'regexp-quote strs))))

(defconst control-chars (append (number-sequence 0   31)
                                ;(number-sequence 127 159)
                                )
  "ASCII control characters")

(defun strip-control-chars (str)
  (apply 'strip-str (cons str (mapcar 'string control-chars))))
