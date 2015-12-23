(asdf:defsystem :transit
  :name "transit"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "MIT"
  :depends-on (:json-streams
               :alexandria
               :string-case
               :parse-float
               :cl-base64
               :local-time)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "cl-transit")))))
