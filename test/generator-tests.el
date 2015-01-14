(test-with gen-num
  "Number generator"
  (list-of 'gen-num)
  (lambda (n)
    (should (numberp n))
    (should (>= n 0))))

(test-with gen-char
  "Character generator"
  (list-of 'gen-char)
  (lambda (c)
    (should (equal 1 (length c)))
    (should (stringp c))))

(test-with gen-string
  "String generator"
  (lambda () (list (gen-string) (random 255)))
  (lambda (s n)
    (should (stringp s))
    (should (equal (format "%s" s) s))
    (should (equal (length (gen-string n)) n))))

(test-with gen-list
  "List generator"
  (lambda ()
    (list (gen-list (lambda () nil))
          (random 255)))
  (lambda (lst n)
    (should (listp lst))
    (should (equal (length (gen-list 'gen-bool n)) n))))

(test-with gen-pair
  "Test pair generation"
  (lambda ())
  (lambda ()
    (let ((sb (gen-pair 'gen-string 'gen-bool))
          (bs (gen-pair 'gen-bool 'gen-string)))
      (should (stringp  (car sb)))
      (should (booleanp (cdr sb)))
      (should (booleanp (car bs)))
      (should (stringp  (cdr bs))))))

(test-with gen-types-id
  "Test types_id value generation"
  (list-of 'gen-types-id)
  (lambda (alist)
    (should (listp alist))
    (dotimes (n (length alist))
      (let* ((type (car (nth n alist)))
             (id   (cdr (assoc type alist))))
        (should (equal (assoc type alist)
                       (cons type id)))))))

(test-with gen-filtered
  "Test generator filters"
  (lambda ()
    (let ((sep (gen-char)))
      (list `(lambda (x) (not (search ,sep x))))))
  (lambda (filter)
    (should (funcall filter
                     (gen-filtered 'gen-string filter)))))

(test-with gen-list-of-simple
  "Test the way list-of works"
  nil
  (lambda ()
    (should (functionp (list-of 'gen-bool)))))

(test-with gen-list-of-callable
   "Test we can call list-of generators"
   nil
   (lambda ()
     (let ((f (list-of (lambda () t))))
       (should (equal (funcall f) (list t))))))

(test-with gen-list-of-symbolic
  "Test we can generate lists with symbolic function names"
  nil
  (lambda ()
    (let* ((f   (list-of 'gen-bool))
           (val (funcall f)))
      (should (or (equal val (list nil))
                  (equal val (list t)))))))

(test-with gen-list-of-multiple
  "Test generating lists"
  nil
  (lambda ()
    (let ((generated (funcall (list-of 'gen-bool 'gen-string 'gen-num))))
      (should (equal (length generated) 3))
      (should (booleanp (nth 0 generated)))
      (should (stringp  (nth 1 generated)))
      (should (numberp  (nth 2 generated))))))
