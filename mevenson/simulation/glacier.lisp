(in-package glacier)

(defun json-parameters ()
  `($.consensus_settings.glacier.evidence_alpha
    4/5
    $.consensus_settings.glacier.evidence_alpha_2
    2/5
    $.consensus_settings.glacier.look_ahead
    997
    $.consensus_settings.query.initial_query_size
    100
    $.consensus_settings.query_multiplier
    2
    $.consensus_settings.max_multiplier
    1
    $.byzantine_settings.total_size
    ,(expt 10 4)
    $.byzantine_settings.honest
    1
    $.distribution.yes
    1/2
    $.distribution.no
    1/2))

(defun jsown-template ()
  '(:OBJ
    ("consensus_settings" :OBJ
     ("glacier" :OBJ ("evidence_alpha" . 4/5) ("evidence_alpha_2" . 1/2)
      ("look_ahead" . 20)
      ("query" :OBJ ("query_size" . 7) ("initial_query_size" . 7)
       ("query_multiplier" . 2) ("max_multiplier" . 4))))
    ("distribution" :OBJ ("yes" . 1/2) ("no" . 1/2) ("none" . 0))
    ("byzantine_settings" :OBJ ("total_size" . 1000)
     ("distribution" :OBJ ("honest" . 1) ("infantile" . 0) ("random" . 0)
      ("omniscient" . 0)))
    ("wards" (:OBJ ("time_to_finality" :OBJ ("ttf_threshold" . 2))))
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
     (uiop:run-program '("./target/release-opt/consensus-simulations"
                         "--input-settings" "etc/glacier.json"
                         "--output-file" "var/glacier.output")) ;; name the output something sensible
     result
     template)))


