(defsystem glacier
  :version "0.0.1"
  :depends-on (alexandria
	       drakma
               jsown
               split-sequence)
  :components ((:module source
                :pathname "./"
                :components ((:file "package")
                             (:file "json-path")
                             (:file "plot")
                             (:file "glacier")))))
                
