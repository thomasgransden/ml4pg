(test-with gen-num
  "Number generator"
  (list-of (gen-num))
  (lambda (n)
    (should (numberp n))
    (should (>= n 0))))

(test-with gen-char
  "Character generator"
  (list-of (gen-char))
  (lambda (c)
    (should (equal 1 (length c)))
    (should (stringp c))))

(test-with gen-char-nonempty
  "Characters from a string"
  (compose (lambda (s)
             (list (split-string s "" t)
                   (funcall (gen-char s))))
           (gen-nonempty-string))
  (lambda (chars char)
    (should (member char chars))))

(test-with gen-string
  "String generator"
  (lambda ()
    (let ((n (funcall (gen-num))))
      (funcall (list-of (gen-string)
                        (gen-string n)
                        (gen-const  n)))))
  (lambda (s1 s2 n)
    (should (stringp s1))
    (should (stringp s2))
    (should (equal (format "%s" s1) s1))
    (should (equal (length s2) n))))

(test-with gen-list
  "List generator"
  (lambda ()
    (let ((n (funcall (gen-num))))
      (funcall (list-of (gen-list (gen-const nil))
                        (gen-const n)
                        (gen-list (gen-bool) n)))))
  (lambda (lst1 n lst2)
    (should (listp lst1))
    (should (listp lst2))
    (should (equal (length lst2) n))))

(test-with gen-pair
  "Test pair generation"
  (list-of (gen-pair (gen-string) (gen-bool))
           (gen-pair (gen-bool)   (gen-string)))
  (lambda (sb bs)
    (should (stringp  (car sb)))
    (should (booleanp (cdr sb)))
    (should (booleanp (car bs)))
    (should (stringp  (cdr bs)))))

(test-with gen-types-id
  "Test types_id value generation"
  (list-of (gen-types-id))
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
    (let* ((sep     (funcall (gen-char)))
           (filter `(lambda (x) (not (search ,sep x)))))
      (list filter
            (funcall (gen-filtered (gen-string) filter)))))
  (lambda (filter output)
    (should (funcall filter output))))

(test-with gen-list-of-simple
  "Test the way list-of works"
  nil
  (lambda ()
    (should (functionp (list-of (gen-bool))))))

(test-with gen-list-of-callable
   "Test we can call list-of generators"
   (list-of (gen-string))
   (lambda (x)
     (let ((f (list-of (gen-const x))))
       (should (equal (funcall f) (list x))))))

(test-with gen-list-of-multiple
  "Test generating lists"
  (list-of (gen-bool) (gen-string) (gen-num))
  (lambda (&rest generated)
    (should (equal (length generated) 3))
    (should (booleanp (nth 0 generated)))
    (should (stringp  (nth 1 generated)))
    (should (numberp  (nth 2 generated)))))

(test-with gen-coq-correct-theorem
  "Test we can generate theorems with valid proofs"
  (list-of (gen-coq-correct-theorem))
  (lambda (str)
    (should (coqp str))))

(test-with gen-coq-name
  "Test Coq names are valid"
  (list-of (gen-coq-name))
  (lambda (n)
    (should (coq-namep n))))

(test-with gen-string-without
  "Test that gen-string-without works"
  ;; Mod 20 since too many constraints makes filtering very slow
  (list-of (gen-list (gen-nonempty-string)
                     (compose (lambda (n)
                                (% n 20))
                              (gen-num))))
  (lambda (strs)
    (message "TRYING WITH '%S'" strs)
    (let ((result (funcall (apply 'gen-string-without strs))))
      (message "GOT '%s'" result)
      (should-not (equal "" result))
      (dolist (str strs)
        (should-not (search str result))))))

(test-with gen-readable
  "Output of gen-readable should be readable without error"
  (list-of (gen-readable))
  (lambda (str)
    (read str)))
