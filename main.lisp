(in-package :corona)

(defparameter *url-csv-casos* "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/reporte-covid/dataset_reporte_covid_sitio_gobierno.csv")

(defparameter *meses* '("JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))

(define-condition fecha-mal-formateada (error) ())

(defun fecha-sin-hora (fecha-con-hora)
  "Le quita la hora al formato de fecha que están usando en el csv
   Fecha (con hora) de ejemplo: 06SEP2020:00:00:00"
  (first (uiop:split-string fecha-con-hora :separator "::")))

(defun indicador (casos-diarios)
  (format nil "[~a] ~4d" (first casos-diarios) (round (second casos-diarios))))

(defun a-timestamp (fecha)
  "Parsea el formato que están usando en el csv a un Universal-time
   Fecha (sin hora) de ejemplo: 06SEP2020"
  (let* ((dia (subseq fecha 0 2))
         (mes (subseq fecha 2 5))
         (anio (subseq fecha 5)))
    (let ((indice-mes (position mes *meses* :test #'string=)))
      (unless indice-mes (error 'fecha-mal-formateada))
      (date-time-parser:parse-date-time (format nil "~a-~2,'0d-~a" anio (1+ indice-mes) dia)))))

(defun caso-para-fecha (casos fecha)
  (handler-case (list (a-timestamp fecha) fecha (gethash fecha casos))
    (fecha-mal-formateada () (progn (format t "Fecha mal formateada: ~a~%" fecha) nil))))

(defun a-lista-de-casos (casos)
  (loop :for (timestamp fecha cantidad-casos)
        :in (sort (loop :for fecha :being :the :hash-key :of casos :for caso := (caso-para-fecha casos fecha) :when caso :collect :it) #'< :key #'first)
        :collecting (list fecha cantidad-casos)))

(defun parsear-casos (archivo-casos)
  (let ((casos (make-hash-table :test 'equal)))
    (cl-csv:do-csv (fila archivo-casos :skip-first-p t)
      (let ((fecha (fecha-sin-hora (nth 0 fila)))
            (tipo-dato (nth 2 fila))
            (subtipo-dato (nth 3 fila))
            (cantidad-casos (parse-float:parse-float (nth 4 fila))))
        (when (and (string= "casos_residentes" tipo-dato) (string= "casos_confirmados_reportados_del_dia" subtipo-dato))
          (setf (gethash fecha casos) (+ cantidad-casos (gethash fecha casos 0))))))
    casos))

(defun descargar-y-mostrar-grafico ()
  (uiop:with-temporary-file (:pathname csv-casos :suffix ".covid19.csv")
    (trivial-download:download *url-csv-casos* csv-casos :quiet t)
    (let ((lista-casos (a-lista-de-casos (parsear-casos csv-casos))))
      (format t (cl-spark:vspark lista-casos :key #'second
                                             :min 0
                                             :title "Casos de COVID para residentes de CABA"
                                             :labels (mapcar #'indicador lista-casos))))))

(defun main ()
  (handler-case (descargar-y-mostrar-grafico)
    (trivial-download:http-error () (print "No se pudo descargar desde el servidor del GCBA"))
    (usocket:ns-try-again-condition () (print "No hay conexión a internet"))
    (sb-sys:interactive-interrupt () (sb-posix:exit 0))))
