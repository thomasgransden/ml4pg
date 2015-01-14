(defun list-of (&rest gens)
  `(lambda ()
     (mapcar 'funcall ',gens)))

(defun gen-bool ()
  "Generate t or nil"
  (equal (random 2) 0))

(defun gen-num ()
  "Generate positive random numbers"
  (random 255))

(defun gen-char ()
  "Generate a random ASCII character"
  (format "%c" (random 255)))

(defun gen-string (&optional op-len)
  "Generate a random ASCII string, of given (or random) length"
  (let ((len (or op-len (random 255)))
        (str ""))
    (dotimes (i len str)
      (setq str (concat str (gen-char))))))

(defun gen-nonempty-string ()
  "Generate a random ASCII string of at least one char"
  (gen-string (1+ (random 254))))

(defun gen-list (elem-gen &optional op-len)
  "Generate a random list, using the given element-generating function, of the
   given (or random) length"
  (let ((len (or op-len (random 255)))
        (lst nil))
    (dotimes (i len lst)
      (setq lst (cons (funcall elem-gen) lst)))))

(defun gen-pair (first second)
  (cons (funcall first) (funcall second)))

(defun gen-types-id ()
  "Generator for types_id values"
  (gen-list (lambda () (gen-pair 'gen-string 'gen-num))))

(defun gen-filtered (elem-gen filter)
  "Filters a generator using a predicate"
  (let ((val (funcall elem-gen)))
    (while (not (funcall filter val))
      (setq val (funcall elem-gen)))
    val))

(defun gen-any (&rest gens)
  "Generate using any one of the arguments, randomly"
  (funcall (nth (random (length gens)) gens)))

(defun gen-string-without (str)
  "Generate a string which doesn't contain STR"
  (gen-filtered 'gen-nonempty-string `(lambda (x) (not (search ,str x)))))
