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

(test-with name-from-buf
  "Test getting a filename from a buffer"
  (lambda ()
    (let* ((nodot  (lambda (x) (not (search "." x))))
           (prefix (gen-filtered 'gen-nonempty-string nodot)))
      (list prefix
            (concat prefix "." (gen-filtered 'gen-nonempty-string nodot)))))
  (lambda (prefix full)
    (with-temp-buffer
      (let ((name (rename-buffer full t)))
        (should (equal (name-from-buf)
                       (if (search "." name)
                           (car (split-string name "."))
                           name)))))))
