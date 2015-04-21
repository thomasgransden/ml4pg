;; Test feature extraction

(test-with numbers-move-arrow
  "Test what numbers-move=> does"
  (list-of (gen-string)
           (gen-string)
           (gen-num)
           (gen-num))
  (lambda (cmd-pre cmd-post top level)
    (let ((cmd (concat cmd-pre "=>" cmd-post)))
      (numbers-move=> cmd top level))))

(test-with remove-arrow
  "Test what remove=> does"
  (list-of (gen-string) (gen-string))
  (lambda (str1 str2)
    (let ((str (concat str1 "=>" str2)))
      (remove=> (subseq str (+ 2 (search "=>" str)))))))
