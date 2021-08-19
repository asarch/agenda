(defpackage :elementos
  (:use :common-lisp)
  (:export
   :text-area
   :input-text
   :check-box
   :renderear
   :formulario
   :indice
   :dropdown-list
   :rating
   :agregar-elemento))

(in-package :elementos)

;; TODO:

;;  - En lugar de hacer name=\"~a\" se hace name=~s, las comillas van implicitas.
;;  - Se hace <label>~a</label>
;;  - En inidice, como hariamos: <td class="center aligned">
;;  - (render #P"indice.html" (list :indice (renderear el-indice)))
;;  - En Smalltalk (Squeak) a los "elementos" se les conoce como "componentes".

;;---------------------------------------------------------------------
;;  Elemento HTML
;;---------------------------------------------------------------------

(defclass elemento-html ()
  ((nombre :initform "" :initarg :nombre :accessor nombre)
   (etiqueta :initform "" :initarg :etiqueta :accessor etiqueta)
   (valor :initform "" :initarg :valor :accessor valor)
   (clase :initform "field" :initarg :clase :accessor clase)))

(defgeneric renderear (instance))

(defmethod renderear ((instance elemento-html))
  (format t "Hello, from elemento-html renderear-elemento!~%"))

;;---------------------------------------------------------------------
;;  Text Area
;;---------------------------------------------------------------------

(defclass text-area (elemento-html) ())

(defmethod renderear ((instance text-area))
  (format nil "<div class=~s><label>~a</label><textarea name=~s>~a</textarea></div>"
	  (clase instance)
	  (etiqueta instance)
	  (nombre instance)
	  (valor instance)))

;;---------------------------------------------------------------------
;;  Input Text
;;---------------------------------------------------------------------

(defclass input-text (elemento-html) ())

(defmethod renderear ((instance input-text))
  (format nil "<div class=~s><label>~a</label><div class=\"ui icon input\"><input class=\"user-input-field\" type=\"text\" name=~s value=~s><i class=\"delete-icon times link icon\"></i></div></div>"
	  (clase instance)
	  (etiqueta instance)
	  (nombre instance)
	  (valor instance)))

;;---------------------------------------------------------------------
;;  Check Box
;;---------------------------------------------------------------------

(defclass check-box (elemento-html) ())

(defmethod renderear ((instance check-box))
  ;; <div class="field"><div class="ui T checkbox"><input type="checkbox" name="publico" T><label>Público</label></div></div>
  ;; <div class="field"><div class="ui checked checkbox"><input type="checkbox" name="publico" T><label>Público</label></div></div>
  ;; Si el cuadro esta seleccionado, es de clase "ui checked checkbox" y debe de ir la propiedad checked=""
  
  (format nil "<div class=~s><div class=\"ui ~a checkbox\"><input type=\"checkbox\" name=~s ~a><label>~a</label></div></div>"
	  (clase instance)
	  (when (valor instance) (format nil "checked"))
	  (nombre instance)
	  (when (valor instance) (format nil "checked=\"\""))
	  (etiqueta instance)))

;;---------------------------------------------------------------------
;;  Dropdown List
;;---------------------------------------------------------------------

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

;;---------------------------------------------------------------------
;;  Rating
;;---------------------------------------------------------------------

(defclass rating (elemento-html)
  ((data-rating :initform 3 :initarg :data-rating :accessor data-rating)
   (data-max-rating :initform 5 :initarg :data-max-rating :accessor data-max-rating)))

;; nombre : calificacion
;; etiqueta : "Calificación"
;; valor : 3

(defmethod renderear ((instance rating))
  (format nil "<div class=~s><label>~a</label><input hidden id=\"rating\" type=\"text\" name=~s value=\"~a\"><div class=\"ui star rating\" data-rating=\"~d\" data-max-rating=\"~d\"></div></div>"
	  (clase instance)
	  (etiqueta instance)
	  (nombre instance)
	  (valor instance)
	  ;;(data-rating instance)
	  (valor instance)
	  (data-max-rating instance)))

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

(defgeneric renderear (formulario))
(defgeneric agregar-elemento (instance elemento))

(defmethod renderear ((instance formulario))
  (let ((buffer (format nil "<form class=~s name=~s action=~s method=~s>"
  			(clase instance)
  			(nombre instance)
  			(accion instance)
  			(metodo instance))))
    (loop for elemento across (elementos instance)
	  do (setf buffer (concatenate 'string buffer (renderear elemento))))
    (setf buffer (concatenate 'string buffer "<input class=\"ui button\" type=\"reset\" value=\"Reset\"><input class=\"ui primary button\" type=\"submit\" value=\"Submit\"></form>"))))

(defmethod agregar-elemento ((instance formulario) elemento)
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
   (accion-agregar :initform nil :initarg :accion-agregar :accessor accion-agregar)
   (placeholder-buscar :initform "" :initarg :placeholder-buscar :accessor placeholder-buscar)
   (columna-modificar :initform nil :initarg :columna-modificar :accessor columna-modificar)  ;; Donde modificas la nota
   (columna-eliminar :initform nil :initarg :columna-eliminar :accessor columna-eliminar)     ;; Donde eliminas la nota
   (columna-acceso :initform nil :initarg :columna-acceso :accessor columna-acceso)           ;; Donde abres el anime por su url
   (columna-extra :initform nil :initarg :columna-extra :accessor columna-extra)))            ;; Algo extra que deseas agregar

(defgeneric renderear (instance))

(defmethod renderear ((instance indice))
  (let ((columnas-extras 0)
	(buffer (format nil "<div class=\"ui segment\"><div class=\"ui two column very relaxed grid\"><div class=\"middle aligned column\"><h2 class=\"ui header\">~a</h2></div><div class=\"right aligned column\"><form method=\"POST\" action=~s><div class=\"ui left icon action input\"><i class=\"search icon\"></i><input type=\"text\" name=\"criterio\" placeholder=~s><button class=\"ui blue submit button\">Buscar</button></div></form></div></div></div>"
			(titulo instance)
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
	       (when (columna-eliminar instance)
		 (setf buffer
		       (concatenate 'string buffer
				    (format nil "<td class=\"center aligned\"><button class=\"ui compact icon button\" onclick=\"confirmacion('~a');\"><i class=\"trash icon\"></i></button></td>"
					    (format nil (columna-eliminar instance) id))))))
	     (setf buffer (concatenate 'string buffer "</tr>")))
    (setf buffer (concatenate 'string buffer "</tbody>"))
    ;; Base -----------------------------------------------------------
    (setf buffer (concatenate 'string buffer (format nil "<tfoot><tr><th colspan=\"~a\"><div class=\"ui right floated pagination menu\"><a class=\"icon item\"><i class=\"left chevron icon\"></i></a><a class=\"item\">1</a><a class=\"item\">2</a><a class=\"item\">3</a><a class=\"item\">4</a><a class=\"item\">5</a><a class=\"icon item\"><i class=\"right chevron icon\"></i></a>" (+ (length (encabezado instance)) columnas-extras))))
    (when (accion-agregar instance)
      (setf buffer (concatenate 'string buffer (format nil "<a class=\"item\" href=~s><i class=\"edit outline icon\"></i></a></div>" (accion-agregar instance)))))
    (setf buffer (concatenate 'string buffer "</th></tr></tfoot></table>"))))
