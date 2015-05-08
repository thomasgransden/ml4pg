(defun join-strings (strs sep)
  "Concatenate STRS together, separated by SEP"
  (mapconcat 'identity strs sep))

(defun any-to-string (any)
  (format "%S" any))

(defun compose (&rest funcs)
  (unless funcs (error "Nothing to compose"))
  `(lambda (&rest args)
     (let ((result args))
       (dolist (func (reverse ',funcs) (car result))
         (setq result (list (apply func result)))))))

(defun strip-regexp (str &rest res)
  (let ((result str))
    (dolist (to-strip res result)
      (setq result (replace-regexp-in-string to-strip "" result)))))

(defun strip-str (str &rest strs)
  (apply 'strip-regexp (cons str (mapcar 'regexp-quote strs))))
