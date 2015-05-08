(defmacro append-to (name val)
  `(setf ,name (append ,name (list ,val))))

(defun random-elem (list)
  (when list (nth (random (length list)) list)))

(defun choose-distinct (size &optional num)
  "Generate a list of length NUM, containing distinct random numbers, each less
   than SIZE.
   SIZE must be >= 1. If non-nil, NUM must be >= 1 and <= SIZE. If nil, a random
  number is used."
  (let ((n      (or num (1+ (random size))))
        (result nil))
    (when (< size 1)
      (error "Can't choose from %s" size))
    (when (or (< n 1) (> n size))
      (error "Can't choose %s numbers less than %s" n size))
    (dotimes (i n result)
      ;; Choose a random number from 1 to (SIZE - i), since i possibilities have
      ;; already been taken
      (let ((x (1+ (random (- size i)))))
        ;; Step over preceding choices
        (setq x (bump-to-above x 0 result))
        ;; Add the new value to result
        (append-to result x)))))

(defun choose-partitions (size &optional num)
  "Choose random positive numbers which sum to SIZE"
  (let ((result nil)  ;; Resulting list of numbers
        (prev   0))   ;; The previous number we saw
    (dolist (elem
             ;; Loop over some distinct random numbers, in order, less than SIZE
             (sort (choose-distinct size num) '<))
      ;; Keep the distances between the random numbers
      (append-to result (- elem prev))
      (setq prev elem))
    ;; Treat any remainder as another chunk
    (let ((diff (- size prev)))
      (when (> diff 0)
        (if (and num (= num (length result)))
            (setq result (append (take-n (1- num) result)
                                 (list (+ diff (car (last result))))))
            (append-to result diff))))
    result))
