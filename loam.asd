(defsystem "loam"
  :version "0.0.1"
  :author "John Jacobsen"
  :license ""
  :depends-on ("cl-charms" "cl-oju" "beast")
  :serial t
  :components ((:file "package")
               (:file "screen")
               (:file "main"))
  :description "A First Game")
