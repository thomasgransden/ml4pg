(test-with gen-num
           "Number generator"
           (lone 'gen-num)
           (lambda (n)
             (should (numberp n))
             (should (>= n 0))))

(test-with gen-char
           "Character generator"
           (lone 'gen-char)
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
           (lone 'gen-types-id)
           (lambda (alist)
             (should (listp alist))
             (dotimes (n (length alist))
               (let* ((type (car (nth n alist)))
                      (id   (cdr (assoc type alist))))
                 (should (equal (assoc type alist)
                                (cons type id)))))))

(test-with gen-filtered
           "Test generator filters"
           (lambda ())
           (lambda ()
             (let ((test-len (lambda (s) (> 100 (length s)))))
               (should (funcall test-len
                                (gen-filtered 'gen-string test-len))))))
