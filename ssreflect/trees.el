;;++++++++++++++++++++++++++++++++++++++++++++++++++
;; Drawing trees
;;++++++++++++++++++++++++++++++++++++++++++++++++++

(defvar treepos 0)

(defun treetographvizaux (parent tree p)
  (do ((temp tree (cdr temp))
       (res ""))
      ((endp temp) res)
    (if (listp (car temp))
    (progn (setf treepos (1+ treepos))
           (setf res (concat res
                 (format "%s[label=\"%s\"];\n" p parent)
                 (format "%s -> %s[arrowhead=none];\n" p treepos)
                 (treetographvizaux (caar temp) (cdar temp) treepos))))
      (progn (setf treepos (1+ treepos))
         (setf res (concat res
                   (format "%s[label=\"%s\"];\n" p parent)
                   (format "%s -> %s[arrowhead=none];\n" p treepos)
                   (format "%s[label=\"%s\"];\n" treepos (car temp))))))))

(defun treetographviz (tree)
  (progn (setf treepos 0)
     (concat "digraph G {" (treetographvizaux (car tree) (cdr tree) 0) "\n}")))

(defun show-diagram-tree (text)
  (let ((png  (process-with-cmd "dot" text nil "-Tpng"))
        (proc (start-process "display" nil "display" "-")))
    (process-send-string proc png)
    (process-send-eof proc)))

(defun showtreegraph (tree)
  (show-diagram-tree (treetographviz tree)))
