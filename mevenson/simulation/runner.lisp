(in-package :glacier)

(defparameter +consensus-simulations+
  (merge-pathnames
   "work/consensus-prototypes/target/release-opt/consensus-simulations"
   (user-homedir-pathname)))

(defun run (trials
            &key
              parameters
              (jsown (jsown-template)))
  "Run the Glacier simulation TRIALS times for PARAMETERS

PARAMETERS are a list of (JSON-PATH VALUE) pairs to get in the JSOWN
template."
  (when parameters
    (loop :for (path value) :in parameters
          :doing
             (set-path jsown path value)))
  (let* ((parameter-string
	   (encode-parameters jsown))
	 (machine 
	   (machine-instance))
         (time
           (format nil "~a" (get-universal-time)))
         (separator
           (string +filename-record-separator+))
	 (base
	   (concatenate 'string
                        parameter-string separator
                        machine separator
                        time))
	 (input-settings
	    (merge-pathnames
	     (concatenate 'string "var/" base ".json")
	     (user-homedir-pathname)))
	 (output-file
	    (merge-pathnames
	     (concatenate 'string "var/" base ".out")
	     (user-homedir-pathname))))
    (alexandria:write-string-into-file
     (jsown:to-json jsown)
     input-settings
     :if-exists :supersede)

    (format *standard-output*
            "~&Runnning ~a trials across ~a nodes for ~a rounds~
~&k=~a l=~a a1=~a a2=~a~%~tyes=~a no=~a~%"
            trials
            (get-path jsown '$.byzantine_settings.total_size)
            (get-path (first (get-path jsown '$.wards)) '$.time_to_finality.ttf_threshold)
            (get-path jsown '$.consensus_settings.glacier.query.initial_query_size)
            (get-path jsown '$.consensus_settings.glacier.look_ahead)
            (get-path jsown '$.consensus_settings.glacier.evidence_alpha)
            (get-path jsown '$.consensus_settings.glacier.evidence_alpha_2)
            (get-path jsown '$.distribution.yes)
            (get-path jsown '$.distribution.no))
            
    (loop :for i :from 1 :upto trials
          :doing
             (let* ((output
                      (make-pathname :defaults output-file
                                     :name (format nil "~a~a~a"
                                                   (pathname-name output-file)
                                                   (string +filename-record-separator+)
                                                   i)))
                    (stdout
                      (make-pathname :defaults output
                                     :type "stdout"))
                    (stderr
                      (make-pathname :defaults output
                                     :type "stderr")))
               (uiop:run-program
                `(,(namestring +consensus-simulations+)
	          "--input-settings" ,(namestring input-settings)
	          "--output-file" ,(namestring output))
                :ignore-error-status t
                :error-output stderr
                :output stdout)
               (format *standard-output* ".")))
    (format *standard-output* "done~%")
             
    (values 
	base)))	       

(defun search-parameters ()
  "A parameter search run"
  ;; not exactly sure why something like
  ;; (loop :for yes :from 0.4 :to 0.6 :by 0.05… has rounding problems
  ;;
  (loop :for yes :from 2/5 :to     3/5 :by 1/20
        :for no  :from 3/5 :downto 2/5 :by 1/20
        :with rounds = 10000
        :doing
           (run 10
                :parameters
                `(($.byzantine_settings.total_size ,(expt 10 4))
                  ($.distribution.yes ,(coerce yes 'single-float))
                  ($.distribution.no ,(coerce no 'single-float))))))
  
