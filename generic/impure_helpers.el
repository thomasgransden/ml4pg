(defun random-elem (list)
  (when list (nth (random (length list)) list)))
