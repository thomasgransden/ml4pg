(require 'image-dired)

;; FIXME: Name conflict
(defun extract-theorem-proof2 (file name)
  (extract-theorem-proof2-aux "ssreflect" file name))
