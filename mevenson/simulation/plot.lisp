(in-package :glacier)

(defun summarize (file)
  "Summarize the contents of polars FILE as per round undecided, no, and yes votes"
  (let* ((s
           (alexandria:read-file-into-string file))
         (j
           (jsown:parse s))
         (rounds
           (loop :for column :in (get-path j '$.columns)
                 :collecting (get-path column '$.values))))
    (loop :for round :in rounds
          :collecting ;; TODO optimize
          (list 
           (length
            (remove-if-not #'zerop round))
           (length
            (remove-if-not
             (lambda (x) (= 1 x))
             round))
           (length 
            (remove-if-not
             (lambda (x) (= 2 x))
             round))))))

(defun summarize-all (&key (directory #p"~/var/"))
  "Summarize all data files in DIRECTORY"
  (let ((files (directory (merge-pathnames "*.out" directory))))
    (dolist (file files)
      (format *standard-output* "~&~a~&~a~%" file (summarize file)))
    (format *standard-output* "~&Summarized ~d files." (length files))))
        
        

    
