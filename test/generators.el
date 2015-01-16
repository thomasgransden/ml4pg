(defun list-of (&rest gens)
  `(lambda ()
     (mapcar 'funcall ',gens)))

(defun gen-bool ()
  "Generate t or nil"
  (lambda () (equal (random 2) 0)))

(defun gen-num ()
  "Generate positive random numbers"
  (lambda () (random ml4pg-test-complexity)))

(defun gen-char ()
  "Generate a random ASCII character"
  (lambda () (format "%c" (random 255))))

(defun gen-string (&optional op-len)
  "Generate a random ASCII string, of given (or random) length"
  (let ((len (cond ((functionp op-len)  op-len)
                   ((null      op-len) (gen-num))
                   (t                  (gen-const op-len)))))
    `(lambda ()
       (let ((str ""))
         (dotimes (i (funcall ,len) str)
           (setq str (concat str (funcall (gen-char)))))))))

(defun gen-nonempty-string ()
  "Generate a random ASCII string of at least one char"
  (gen-string (1+ (funcall (gen-num)))))

(defun gen-list (elem-gen &optional op-len)
  "Generate a random list, using the given element-generating function, of the
   given (or random) length"
  (let ((len (cond ((functionp op-len)  op-len)
                   ((null      op-len) (gen-num))
                   (t                  (gen-const op-len)))))
    `(lambda ()
       (let (lst)
         (dotimes (i (funcall ,len) lst)
           (setq lst (cons (funcall ,elem-gen) lst)))))))

(defun gen-pair (first second)
  `(lambda ()
     (cons (funcall ,first) (funcall ,second))))

(defun gen-types-id ()
  "Generator for types_id values"
  (gen-list (gen-pair (gen-string) (gen-num))))

(defun gen-filtered (elem-gen filter)
  "Filters a generator using a predicate"
  `(lambda ()
     (let ((val (funcall ,elem-gen)))
       (while (not (funcall ,filter val))
         (setq val (funcall ,elem-gen)))
       val)))

(defun gen-any (&rest gens)
  "Generate using any one of the arguments, randomly"
  (unless gens (error "No generators to choose between"))
  `(lambda ()
     (funcall (random-elem ,gens))))

(defun gen-string-without (str)
  "Generate a string which doesn't contain STR"
  (gen-filtered (gen-nonempty-string) `(lambda (x) (not (search ,str x)))))

(defun gen-elem (lst)
  "Generate an element of LST"
  (unless lst (error "Cannot generate elements from an empty list"))
  `(lambda ()
     (random-elem ',lst)))

(defun gen-const (&rest args)
  "Generate one of the arguments"
  (unless args (error "No constants to generate"))
  (gen-elem args))
