;; Pure functions for manipulating graphs

(defun clusterofseveral-pure (lol tbl defs)
  (clusterofseveral-aux (clusterofone lol 1 tbl defs)))

(defun show-diagram-clusters-aux (text)
  "Turns the given dot code into a PNG with an image map, and return a HTML
   page containing this image"
  (let* ((map (process-with-cmd "dot" text nil "-Tcmap"))
         (png (process-with-cmd "dot" text nil "-Tpng"))
         (b64 (process-with-cmd "base64" png)))
    (createwebpage map b64)))

(defun issubcluster (cluster1 cluster2)
  (let (missing)
    (dolist (elem cluster1 (not missing))
      (setq missing (or missing
                        (not (member elem cluster2)))))))

(defun clusterofone-node (elem tbl defs counter)
  (format "subgraph cluster%s {\n%s\n}\n"
          counter (clusterofone elem (1+ counter) tbl defs)))

(defun clusterofone-leaf (elem tbl defs next)
  (let ((first  (nth-of elem tbl))
        (second (library-belong-aux (1- elem) defs)))
    (if (listp next)
        (format "%s [URL=\"./%s.html\"];\n"
                first second)
      (format "%s [URL=\"./%s.html#%s\"]; %s -> %s[style=invis]\n"
              first second first first (nth-of next tbl)))))

(defun clusterofone (lst counter tbl defs)
  (do ((temp lst (cdr temp))
       (res ""))
      ((endp temp) res)
    (let ((elem (car temp)))
      (concat-to res (if (listp elem)
                         (clusterofone-node elem tbl defs counter)
                       (clusterofone-leaf elem tbl defs
                                          (when (cdr temp) (cadr temp))))))))

(defun createwebpage (map b64)
  (format "<head>
             <title>Dependency Diagram</title>
           </head>
           <body>
             <img src='data:image/png;base64,%s' usemap='#depend' />
             <map id='depend' name='depend'>%s</map>
           </body>" b64 map))

(defun clusterofseveral-aux (txt)
  (concat "digraph {\n rankdir=LR;\n" txt "\n}"))

(defun replacecluster (cluster1 cluster2)
  (if cluster2
      (let ((elem (car cluster2)))
        (if (listp elem)
            (if (issubcluster cluster1 elem)
                (cons (replacecluster cluster1 elem)
                      (cdr cluster2))
              (cons elem
                    (replacecluster cluster1 (cdr cluster2))))
          (if (member elem cluster1)
              (replacecluster cluster1 (cdr cluster2))
            (cons elem (replacecluster cluster1 (cdr cluster2))))))
    (list cluster1)))

(defun replace-subclusters (cluster)
  `(lambda (elem)
     (if (issubcluster ',cluster elem)
         (replacecluster ',cluster elem)
       elem)))

(defun subclusters (cluster clusters)
  "Return a copy of CLUSTERS which contains CLUSTER as an element"
  (cond ((member cluster clusters)
         clusters)

        ((any-which clusters 'issubcluster cluster)
         (mapcar (replace-subclusters cluster) clusters))

        (t
         (append clusters (list cluster)))))

(defun subclustersseveral (clusters1 clusters2)
  (let ((result clusters2))
    (dolist (elem clusters1 result)
      (setf result (subclusters elem result)))))
