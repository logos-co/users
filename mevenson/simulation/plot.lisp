(in-package :glacier)

(defparameter +filename-record-separator+
  #\-)

(defun summarize (file)
  "Summarize the contents of Polars formatted FILE as per round undecided, no, and yes votes"
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

(defun gnuplot (file)
  "Generate gnuplot command and data for FILE"
  (let ((data
          (make-pathname :defaults file
                         :type "data"))
        (plot 
          (make-pathname :defaults file
                         :type "gnuplot"))
        (summary
          (summarize file)))
    (with-open-file (out data :direction :output :if-exists :supersede)
      (loop :for (undecided no yes) :in summary
            :doing (format out "~a ~a ~a~%" undecided no yes)))
    (with-open-file (out plot :direction :output :if-exists :supersede)
      (format out "source = '~a'~%" data)
      (format out "set xlabel 'rounds'~%")
      (format out "set ylabel 'node opinions'~%")
      (format out "plot ~
source using 1 with linespoints pointnumber 3 title 'none', ~
source using 2 with linespoints pointnumber 7 title 'no', ~
source using 3 with linespoints pointnumber 11 title 'yes', ~
1/0 title '~a'~%" (gnuplot-legend file)))
    (values
     plot 
     data)))

(defun gnuplot-legend (filename)
  (let* ((name
           (do-urlencode:urldecode (pathname-name filename) :queryp t))
         (all-pairs
           (first 
            (split-sequence:split-sequence +filename-record-separator+ name)))
         (pairs
           (split-sequence:split-sequence #\space all-pairs))
         (assoc-pairs
           (loop :for pair :in pairs
                 :collecting (let ((key-value (split-sequence:split-sequence #\= pair)))
                               `(,(first key-value) . ,(second key-value))))))
    (flet
        ((get-value (key)
           (alexandria:assoc-value assoc-pairs key :test 'equal)))
      (values
       (format nil "l=~a α_1=~a α_2=~a k=~a"
               (get-value "look_ahead")
               (get-value "evidence_alpha")
               (get-value "evidence_alpha_2")
               (get-value "initial_query_size"))
       assoc-pairs))))
                         


    
