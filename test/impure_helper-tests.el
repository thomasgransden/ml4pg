(test-with append-to
           "Check we can append to variables"
           (lambda () (list (gen-string) (gen-list 'gen-string)))
           (lambda (s l)
             (append-to l s)
             (should (member s l))))
