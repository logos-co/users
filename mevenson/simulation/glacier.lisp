(in-package glacier)

(defun json-parameters ()
  `($.consensus_settings.glacier.evidence_alpha
    0.8 ;;4/5
    $.consensus_settings.glacier.evidence_alpha_2
    0.4 ;;2/5
    $.consensus_settings.glacier.confidence_beta
    0.5 ;;1/2
    $.consensus_settings.glacier.look_ahead
    997
    $.consensus_settings.glacier.query.initial_query_size
    100
    $.consensus_settings.glacier.query.query_multiplier
    2
    $.consensus_settings.glacier.query.max_multiplier
    1
    $.byzantine_settings.total_size
    ,(expt 10 4)
    $.byzantine_settings.distribution.honest
    1
    $.distribution.yes
    0.5 ;;1/2
    $.distribution.no
    0.5))

(defparameter +consensus-simulations+
  (merge-pathnames
   "work/consensus-prototypes/target/release-opt/consensus-simulations"
   (user-homedir-pathname)))

(defun jsown-template ()
  '(:OBJ
    ("consensus_settings"
     :OBJ
     ("glacier"
      :OBJ
      ("evidence_alpha" . 0.8 )
      ("evidence_alpha_2" . 0.5)
      ("confidence_beta" . 1.0)
      ("look_ahead" . 20)
      ("query" :OBJ ("query_size" . 7) ("initial_query_size" . 7)
       ("query_multiplier" . 2) ("max_multiplier" . 4))))
    ("distribution" :OBJ ("yes" . 0.5)  ("no" . 0.5) ("none" . 0))
    ("byzantine_settings" :OBJ ("total_size" . 1000)
     ("distribution"
      :OBJ ("honest" . 1) ("infantile" . 0) ("random" . 0) ("omniscient" . 0)))
    ("wards" (:OBJ ("time_to_finality" :OBJ ("ttf_threshold" . 100))))
    ("network_modifiers" (:OBJ ("random_drop" :OBJ ("drop_rate" . 0))))))

(defun grind ()
  (let* ((template
           (probe-file "~/work/consensus-prototypes/etc/glacier.json"))
         (json
           (alexandria:read-file-into-string template))
         (result
           (jsown:parse json)))
    ;; TODO set the template based on the parameters
    (values
     result
     template)))

;;; N.b. assumes that all JSON keys are 1) lowercase, and 2) unique
(defun encode-parameters (jsown)
  (let* ((key-json-paths
	  (loop :for (json-path _) :on (json-parameters) :by #'cddr
		:collecting json-path))
	 (keys
	   (loop :for key :in key-json-paths
		 :collecting (cons
			      (first (last (parse-json-path key)))
			      key)))
	 (key-values
	   (loop :for (key . path-symbol) :in keys
		 :collecting (cons
			      key
			      (ignore-errors
			       (get-path jsown
					 (parse-json-path path-symbol))))))
	 (string
	   (values
	    (format nil "~{~a ~}"
		    (loop for (key . value) :in key-values
			  :collecting (format nil "~a=~a" key value))))))
    (values
     (drakma:url-encode string :utf8)
     string)))


(defun run (trials &key (jsown (jsown-template)))
  (let* ((parameters
	   (encode-parameters jsown))
	 (id ;; TODO use host-date))
	   "0")
	 (base
	   (format nil "~a-~a" id parameters))
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
            "~&Runnning ~a trials across ~a nodes~
~&k=~a l=~a a1=~a a2=~a~%~tyes=~a no=~a~%"
            trials
            (get-path jsown '$.byzantine_settings.total_size)
            (get-path jsown '$.consensus_settings.glacier.query.initial_query_size)
            (get-path jsown '$.consensus_settings.glacier.look_ahead)
            (get-path jsown '$.consensus_settings.glacier.evidence_alpha)
            (get-path jsown '$.consensus_settings.glacier.evidence_alpha_2)
            (get-path jsown '$.distribution.yes)
            (get-path jsown '$.distribution.no))
            
    (loop :for i :from 1 :upto trials
          :doing
             (let ((output (namestring
                            (make-pathname :defaults output-file
                                           :name (format nil "~a-~a"
                                                         (pathname-name output-file)
                                                         i)))))
               (uiop:run-program
                `(,(namestring +consensus-simulations+)
	          "--input-settings" ,(namestring input-settings)
	          "--output-file" ,(namestring output))
                :ignore-error-status t
                :error-output :string
                :output :string)
               (format *standard-output* ".")))
    (format *standard-output* "done~%")
             
    (values 
	base)))	       


(defun search-parmeters ()
  
