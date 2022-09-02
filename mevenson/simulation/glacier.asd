(defsystem glacier
  :version "0.0.1"
  :depends-on (alexandria
               jsown
               split-sequence)
  :components ((:module source
                :pathname "./"
                :components ((:file "package")
                             (:file "json-path")
                             (:file "glacier")))))
                
