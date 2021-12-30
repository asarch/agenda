(defpackage :elementos
  (:use :common-lisp)
  (:export
   :text-area
   :input-text
   :check-box
   :dropdown-list
   :radio-group
   :archivo
   :acordeon
   :renderear
   :formulario
   :indice
   :rating
   :calendario
   :comentario
   :comentarios
   :agregar
   :url-externo
   :url-externo-encabezado
   :url-informacion
   :agregar-elemento-archivo
   :centrar))

(in-package :elementos)

(defgeneric renderear (instance))

;; Recuerda que aqui dice el numero de argumentos que toma la funcion generica
(defgeneric agregar (instance elemento))

;; TODO:

;;  - Se hace <label>~a</label>
;;  - En Smalltalk (Squeak) a los "elementos" se les conoce como "componentes".
;;  - En AIDA/Web a las partes del paradigma MVC (Model, View, Controller) se les llama "dominios".
;;  - En lugar de name=\"~a\" se hace name=~s

;;---------------------------------------------------------------------
;;  Elemento HTML
;;---------------------------------------------------------------------

(defclass elemento-html ()
  ((nombre :initform "" :initarg :nombre :accessor nombre)
   (etiqueta :initform "" :initarg :etiqueta :accessor etiqueta)
   (valor :initform "" :initarg :valor :accessor valor)
   (clase :initform "field" :initarg :clase :accessor clase)))

(defmethod renderear ((instance elemento-html))
  (format t "Hello, from elemento-html renderear-elemento!~%"))

;; Text Area

(defclass text-area (elemento-html) ())

(defmethod renderear ((instance text-area))
  (format nil "<div class=~s><label>~a</label><textarea name=~s>~a</textarea></div>"
	  (clase instance)
	  (etiqueta instance)
	  (nombre instance)
	  (valor instance)))

;; Input Text

(defclass input-text (elemento-html) ())

(defmethod renderear ((instance input-text))
  (format nil "<div class=~s><label>~a</label><div class=\"ui icon input\"><input class=\"user-input-field\" type=\"text\" name=~s value=~s><i class=\"delete-icon times link icon\"></i></div></div>"
	  (clase instance)
	  (etiqueta instance)
	  (nombre instance)
	  (if (stringp (valor instance)) (cl-ppcre:regex-replace-all "\"" (valor instance) "&quot;")
	      (valor instance))))

;; Check Box

(defclass check-box (elemento-html)
  ;; Tipo..: :check, :slider, :toggle
  ;; Estado: son dos formas para su estado:
  ;; 1. En la clase en div: <div class="ui slider checkbox checked">
  ;; 2. Como propiedad....:
  ;;    a). <input type="radio" name="throughput" checked="checked">
  ;;    b). <input type="radio" name="throughput" checked> <- Mozilla prefiere este estilo
  ((tipo :initarg :tipo :initform :check :accessor tipo)
   (codigo :initarg :codigo :initform nil :accessor codigo)))

(defmethod renderear ((instance check-box))
  ;; <div class="field"><div class="ui T checkbox"><input type="checkbox" name="publico" T><label>Público</label></div></div>
  ;; <div class="field"><div class="ui checked checkbox"><input type="checkbox" name="publico" T><label>Público</label></div></div>
  ;; Si el cuadro esta seleccionado, es de clase "ui checked checkbox" y debe de ir la propiedad checked=""
  (format nil "<div class=~s><div class=\"ui ~a checkbox\"><input type=\"checkbox\" name=~s ~a><label>~a</label></div>~a</div>"
	  (clase instance)
	  (if (string= (valor instance) "") ""
	    (format nil "cheked")) ;; <div class="ui checked checkbox">…</div>
	  (nombre instance)
	  (if (string= (valor instance) "") ""
	    (format nil "checked")) ;; <input type="checkbox" checked>
	  (etiqueta instance)
	  (if (codigo instance) (codigo instance) "")))

;; Dropdown List

(defclass dropdown-list (elemento-html)
  ((elementos :initform nil :initarg :elementos :accessor elementos)
   (descripcion :initform "Seleccione un elemento de la lista" :initarg :descripcion :accessor descripcion)))

(defmethod renderear ((instance dropdown-list))
  (let ((buffer (format nil "<div class=~s><label>~a</label><div class=\"ui search selection dropdown\"><input type=\"hidden\" name=~s value=~s><i class=\"dropdown icon\"></i><div class=\"default text\">~a</div><div class=\"menu\">"
  			(clase instance)
  			(etiqueta instance)
  			(nombre instance)
  			(valor instance)
			(descripcion instance))))
    (loop for elemento in (elementos instance)
	  do (setf buffer (concatenate 'string buffer (format nil "<div class=\"item\" data-value=\"~a\">~a</div>" (getf elemento :id) (getf elemento :nombre)))))
    (setf buffer (concatenate 'string buffer "</div></div></div>"))))

;; Rating

(defclass rating (elemento-html)
  ((data-rating :initform 3 :initarg :data-rating :accessor data-rating)
   (data-max-rating :initform 5 :initarg :data-max-rating :accessor data-max-rating)))

;; nombre : calificacion
;; etiqueta : "Calificación"
;; valor : 3

;; En Semantic UI solamente necesitas hacer:
;;
;; <div class="ui star rating">…</div>
;;
;; En Fomantic-UI especifica el color y el icono que se usara
;; como indicador del valor de la calificaion:
;;
;; <div class="ui yellow rating" data-icon="star" data-rating="3">…</div>

(defmethod renderear ((instance rating))
  (format nil "<div class=~s><label>~a</label><input hidden id=\"rating\" type=\"text\" name=~s value=\"~a\"><div class=\"ui yellow rating\" data-icon=\"star\" data-rating=\"~d\" data-max-rating=\"~d\"></div></div>"
	  (clase instance)
	  (etiqueta instance)
	  (nombre instance)
	  (valor instance)
	  (valor instance)
	  (data-max-rating instance)))

;; Archivo uploader

(defclass archivo (elemento-html) ())

(defmethod renderear ((instance archivo))
  ;; <div class="field">
  ;;   <label>Seleccione el archivo</label>
  ;;   <input name="nombre" type="file" multiple> <- 'multiple' no es soportado en cualquier navegador
  ;; </div>
  (format nil "<div class=~s><label>~a</label><input name=~s type=\"file\"></div>"
	  (clase instance) (etiqueta instance) (nombre instance))) 

;; Acordeon

(defclass acordeon (elemento-html)
  ((elementos :initform (make-array 5 :adjustable t :initial-element "" :element-type 'string :fill-pointer 0) :initarg :elementos :accessor elementos)))

(defmethod agregar ((instance acordeon) elemento)
  (vector-push-extend elemento (elementos instance)))

;; <div class="field">
;;	<div class="ui accordion">
;;		<div class="active title">
;;			<i class="dropdown icon"></i>Subir archivo
;;		</div>
;;		<div class="active content">
;;			<div class="field">
;;				<label>Seleccionar archivo</label>
;;				<input type="file">
;;			</div>
;;		</div>
;;	</div>
;;</div>

(defmethod renderear ((instance acordeon))
  (let ((buffer (format nil "<div class=~s><div class=\"ui accordion\"><div class=\"active title\"><i class=\"dropdown icon\"></i>~a</div><div class=\"active content\">"
			(clase instance) (etiqueta instance))))
    (loop for elemento across (elementos instance)
	  do (setf buffer (concatenate 'string buffer (renderear elemento))))
    (setf buffer (concatenate 'string buffer "</div></div></div>"))))

(defclass calendario (elemento-html)
  ((placeholder :initform "Seleccionar fecha/hora" :initarg :placehloder :accessor placeholder)))

(defmethod renderear ((instance calendario))
  (format nil "<div class=~s><label>~a</label><div class=\"ui calendar\"><div class=\"ui input left icon\"><i class=\"calendar icon\"></i><input type=\"text\" name=~s placeholder=~s value=~s></div></div></div>"
	  (clase instance) (etiqueta instance) (nombre instance) (placeholder instance) (valor instance)))

;; Radio

;; "grouped fields" "inline fields"
(defclass radio-group (elemento-html)
  ((elementos :initform nil :initarg :elementos :accessor elementos)))

;; checked="checked"
(defmethod renderear ((instance radio-group))
  (let ((buffer (format nil "<div class=~s><label>~a</label>" (clase instance) (etiqueta instance))))
    (loop for elemento in (elementos instance) for pos from 0
	  do (setf buffer (concatenate 'string buffer (format nil "<div class=\"field\"><div class=\"ui radio checkbox\"><input type=\"radio\" name=~s value=~s ~a><label>~a</label></div></div>" (nombre instance) (string-downcase elemento) (if (= pos 0) "checked=\"checked\"" "") elemento))))
    (setf buffer (concatenate 'string buffer "</div>"))))

;;---------------------------------------------------------------------
;;  Formulario
;;---------------------------------------------------------------------

(defclass formulario ()
  ((titulo :initform "" :initarg :titulo :accessor titulo)
   (accion :initform "" :initarg :accion :accessor accion)
   (nombre :initform "" :initarg :nombre :accessor nombre)
   (metodo :initform "POST" :initarg :metodo :accessor metodo)
   (clase :initform "ui form" :initarg :clase :accessor clase)
   (elementos :initform (make-array 5 :adjustable t :initial-element "" :element-type 'string :fill-pointer 0) :initarg :elementos :accessor elementos)))

(defmethod renderear ((instance formulario))
  (let ((buffer (format nil "<form class=~s name=~s action=~s method=~s enctype = \"multipart/form-data\">"
  			(clase instance)
  			(nombre instance)
  			(accion instance)
  			(metodo instance))))
    (loop for elemento across (elementos instance)
	  do (setf buffer (concatenate 'string buffer (renderear elemento))))
    (setf buffer (concatenate 'string buffer "<input class=\"ui button\" type=\"reset\" value=\"Reset\"><input class=\"ui primary button\" type=\"submit\" value=\"Submit\"></form>"))))

(defmethod agregar ((instance formulario) elemento)
  (vector-push-extend elemento (elementos instance)))

;;---------------------------------------------------------------------
;;  Indice
;;---------------------------------------------------------------------

(defclass indice ()
  ((titulo :initform "" :initarg :titulo :accessor titulo)
   (claves :initform nil :initarg :claves :accessor claves)
   (elementos :initform nil :initarg :elementos :accessor elementos)
   (encabezado :initform nil :initarg :encabezado :accessor encabezado)
   (accion-buscar :initform "" :initarg :accion-buscar :accessor accion-buscar)
   (metodo-buscar :initform "POST" :initarg :metodo-buscar :accessor metodo-buscar)
   (accion-agregar :initform nil :initarg :accion-agregar :accessor accion-agregar)
   (placeholder-buscar :initform "" :initarg :placeholder-buscar :accessor placeholder-buscar)
   (columna-modificar :initform nil :initarg :columna-modificar :accessor columna-modificar)        ;; Donde modificas la nota
   (columna-eliminar :initform nil :initarg :columna-eliminar :accessor columna-eliminar)           ;; Donde eliminas la nota
   (columna-comentarios :initform nil :initarg :columna-comentarios :accessor columna-comentarios)  ;; Comentarios del elemento
   (columna-acceso :initform nil :initarg :columna-acceso :accessor columna-acceso)                 ;; Donde abres el anime por su url
   (columna-extra :initform nil :initarg :columna-extra :accessor columna-extra)))                  ;; Algo extra que deseas agregar

(defmethod renderear ((instance indice))
  (let ((columnas-extras 0)
	(buffer (format nil "<div class=\"ui segment\"><div class=\"ui two column very relaxed grid\"><div class=\"middle aligned column\"><h2 class=\"ui header\">~a</h2></div><div class=\"right aligned column\"><form method=~s action=~s><div class=\"ui left icon action input\"><i class=\"search icon\"></i><input type=\"text\" name=\"criterio\" placeholder=~s><button class=\"ui blue submit button\">Buscar</button></div></form></div></div></div>"
			(titulo instance)
			(metodo-buscar instance)
			(accion-buscar instance)
			(placeholder-buscar instance))))
    ;; Encabezado -----------------------------------------------------
    (setf buffer (concatenate 'string buffer "<table class=\"ui striped table\"><thead><tr>"))
    (loop for elemento in (encabezado instance)
	  do (setf buffer (concatenate 'string buffer (format nil "<th>~a</th>" elemento))))
    (when (columna-modificar instance)
      (setf columnas-extras (+ columnas-extras 1))
      (setf buffer (concatenate 'string buffer "<th><i class=\"edit icon\"></i></th>")))
    (when (columna-acceso instance)
      (setf columnas-extras (+ columnas-extras 1))
      (setf buffer (concatenate 'string buffer "<th><i class=\"share square icon\"></i></th>")))      
    (when (columna-extra instance)
      (setf columnas-extras (+ columnas-extras 1))
      (setf buffer (concatenate 'string buffer "<th><i class=\"paper plane icon\"></i></th>")))
    (when (columna-comentarios instance)
      (setf columnas-extras (+ columnas-extras 1))
      (setf buffer (concatenate 'string buffer "<th><i class=\"comment alternate icon\"></i></th>")))
    (when (columna-eliminar instance)
      (setf columnas-extras (+ columnas-extras 1))
      (setf buffer (concatenate 'string buffer "<th class=\"center aligned\"><i class=\"trash icon\"></i></th>")))
    (setf buffer (concatenate 'string buffer "</tr></thead>"))
    ;; Cuerpo ---------------------------------------------------------
    (setf buffer (concatenate 'string buffer "<tbody>"))
    (loop for elemento in (elementos instance)
	  do (setf buffer (concatenate 'string buffer "<tr>"))
	     (loop for clave in (claves instance)
		   do (setf buffer (concatenate 'string buffer (format nil "<td>~a</td>" (getf elemento clave)))))
	     (let ((id (getf elemento (car (claves instance)))))
	       (when (columna-modificar instance)
		 (setf buffer
		       (concatenate 'string buffer
				    (format nil "<td><a href=~s><i class=\"edit outline icon\"></i></a></td>"
					    (format nil (columna-modificar instance) id)))))
	       (when (columna-acceso instance)
		 (setf buffer
		       (concatenate 'string buffer
				    (format nil "<td><a href=~s><i class=\"share square outline icon\"></i></a></td>"
					    (format nil (columna-acceso instance) id)))))
	       (when (columna-extra instance)
		 (setf buffer
		       (concatenate 'string buffer
				    (format nil "<td><a href=~s><i class=\"paper plane outline icon\"></i></a></td>"
					    (format nil (columna-extra instance) id)))))
	       (when (columna-comentarios instance)
		 (setf buffer
		       (concatenate 'string buffer
				    (format nil "<td><a href=~s><i class=\"comment alternate outline icon\"></i></a></td>"
					    (format nil (columna-comentarios instance) id)))))
	       (when (columna-eliminar instance)
		 (setf buffer
		       (concatenate 'string buffer
				    (format nil "<td class=\"center aligned\"><button class=\"tiny circular ui icon button\" onclick=\"confirmacion('~a')\"><i class=\"trash icon\"></i></button></td>"
					    (format nil (columna-eliminar instance) id))))))
	     (setf buffer (concatenate 'string buffer "</tr>")))
    (setf buffer (concatenate 'string buffer "</tbody>"))
    ;; Base -----------------------------------------------------------
    (setf buffer (concatenate 'string buffer (format nil "<tfoot><tr><th colspan=\"~a\"><div class=\"ui right floated pagination menu\"><a class=\"icon item\"><i class=\"left chevron icon\"></i></a><a class=\"item\">1</a><a class=\"item\">2</a><a class=\"item\">3</a><a class=\"item\">4</a><a class=\"item\">5</a><a class=\"icon item\"><i class=\"right chevron icon\"></i></a>" (+ (length (encabezado instance)) columnas-extras))))
    (when (accion-agregar instance)
      (setf buffer (concatenate 'string buffer (format nil "<a class=\"item\" href=~s><i class=\"edit outline icon\"></i></a></div>" (accion-agregar instance)))))
    (setf buffer (concatenate 'string buffer "</th></tr></tfoot></table>"))))

;;--------------------------------------------------------------------
;;  Comentarios
;;--------------------------------------------------------------------

(defclass comentario ()
  ((titulo :initform nil :initarg :titulo :accessor titulo)
   (contenido :initform nil :initarg :contenido :accessor contenido)))

(defmethod renderear ((instance comentario))
;;<a class=\"ui right corner label\"><i class=\"pencil alternate icon\"></i></a>
  ;;(format nil "<div class=\"ui mini message\"><div class=\"ui dividing header\">~a</div><pre style=\"white-space: break-spaces;\">~a</pre><div class=\"ui mini basic icon buttons\"><button class=\"ui button\"><i class=\"edit icon\"></i></button><button class=\"ui button\"><i class=\"trash icon\"></i></button></div></div>"
  (format nil "<div class=\"ui mini message\"><div class=\"header\">~a</div><pre style=\"white-space: break-spaces;\">~a</pre></div>"
  	  (titulo instance) (contenido instance)))

(defclass comentarios ()
  ((elementos :initform (make-array 5 :adjustable t :initial-element "" :element-type 'string :fill-pointer 0) :initarg :elementos :accessor elementos)))

(defmethod agregar-elemento ((instance comentarios) elemento)
  (vector-push-extend elemento (elementos instance)))

(defmethod renderear ((instance comentarios))
  (let (buffer)
    (loop for elemento across (elementos instance)
    	  do (setf buffer (concatenate 'string buffer (renderear elemento)))) buffer))

;;---------------------------------------------------------------------
;;  Funciones auxiliares
;;---------------------------------------------------------------------

(defun centrar (elemento)
  (format nil "<div class=\"ui center aligned fluid container\">~a</div>" elemento))

;; Todas estas funciones debe de ir en elementos.lisp

(defun url-informacion (elemento)
  (format nil "<a href=\"/animes/~d/informacion\">~a</a>"
	  (getf elemento :anime) (getf elemento :nombre)))

;; <div class="ui center aligned fluid container">
;;   <a href="…" target="_blank"><i class="external alternate icon"></i></a>
;; </div>
(defun url-externo (elemento)
  (centrar
   (format nil "<a href=~s target=\"_blank\"><i class=\"external alternate icon\"></i></a>"
	   (getf elemento :direccion))))

;; <div class="ui center aligned fluid container">
;;   <i class="external square alternate icon"></i>
;; </div>
(defun url-externo-encabezado ()
  (centrar
   (format nil "<i class=\"external square alternate icon\"></i>")))

;; Debe de irse al archivo 'elementos.lisp'
(defmethod agregar-elemento-archivo ((instance formulario))
  (let ((acordeon (make-instance 'acordeon :etiqueta "Subir archivo")))
    (agregar acordeon (make-instance 'archivo :etiqueta "Seleccionar archivo" :nombre "archivo"))
    (agregar instance acordeon)))
