(defmacro append-to (name val)
  `(setf ,name (append ,name (list ,val))))

(defmacro cons-prepend (name val)
  `(setf ,name (cons ,val ,name)))

(defmacro concat-to (name lst)
  `(setf ,name (concat ,name ,lst)))
