;; Test feature extraction

(test-with can-extract-theorems
  "Extracting theorems runs"
  nil
  (lambda ()
    (message "RUNNING extract-feature-theorems")
    (extract-feature-theorems)))
