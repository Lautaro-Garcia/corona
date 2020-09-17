(asdf:defsystem :corona
  :description "TUI para mostrar un gráfico de los casos diarios de COVID-19 en CABA"
  :author "Lautaro García"
  :version "1.0.0"
  :serial t
  :depends-on (:cl-csv :cl-spark :trivial-download :cl-date-time-parser)
  :components ((:file "package")
               (:file "main")))
