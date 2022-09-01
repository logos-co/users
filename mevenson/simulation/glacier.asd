(defsystem glacier
  :version "0.0.1"
  :depends-on (jsown)
  :components ((:module source
                :pathname "./"
                :components ((:file "glacier")))))
                
