(in-package :glacier)

(defun summarize (file)
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
           
    
