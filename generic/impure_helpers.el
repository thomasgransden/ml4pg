(defmacro append-to (name val)
  `(setf ,name (append ,name (list ,val))))

(defun process-with-cmd (cmd stdin &optional handler &rest args)
  "Run command CMD, with string STDIN as its stdin. ARGS can contain additional
   arguments for CMD. Returns the stdout as a string. If the exist code is
   nonzero, it will be passed to HANDLER. If HANDLER is nil, an error occurs."
  (with-temp-buffer
    (insert stdin)
    (let ((code (apply 'call-process-region (append (list (point-min)
                                                          (point-max)
                                                          cmd
                                                          t
                                                          t
                                                          nil)
                                                    args))))
      (if (equal 0 code)
          (buffer-string)
          (if handler (funcall handler code)
                      (error "Command %s failed with code %s" cmd code))))))

(defun random-elem (list)
  (when list (nth (random (length list)) list)))

(defun coqp (str)
  (let ((res (coqp-aux str)))
    (unless res
      (message "Not valid Coq code: %s" str))
    res))

(defun coqp-aux (str)
  "Check whether STR contains valid Coq code by trying to compile it"
  (let* ((dir (make-temp-file "ml4pg_check_coq" t))
         (f   (concat dir "/file.v")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert str))
          (write-to-messages
           `(lambda ()
              (stringp (process-with-cmd "coqc"
                                         ""
                                         (lambda (&rest x)
                                           (list nil (buffer-string)))
                                         ,f)))))
      (delete-directory dir t nil))))

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
