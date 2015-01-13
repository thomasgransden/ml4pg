(test-with flatten
           "Turn values into 1D lists"
           (lambda ())
           (lambda ()
             (should (equal (flatten nil) nil))
             (should (equal (flatten 10) '(10)))
             (should (equal (flatten '(1 2 3)) '(1 2 3)))
             (should (equal (flatten '((1 2) (((3) 4 (5 (6 7))) 8) 9))
                            '(1 2 3 4 5 6 7 8 9)))))
