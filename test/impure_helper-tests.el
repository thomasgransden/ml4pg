(test-with append-to
           "Check we can append to variables"
           (lambda () (list (gen-string) (gen-list 'gen-string)))
           (lambda (s l)
             (append-to l s)
             (should (member s l))))

(test-with process-with-cmd
           "Test external commands with stdin/out handling"
           (lambda ())
           (lambda ()
             (should (equal "b\nd\nf\ni\n"
                            (process-with-cmd "grep"
                                              "abcd\nefghijk"
                                              "-o"
                                              "[bdfi]")))))
