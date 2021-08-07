(ql:quickload :cl-cffi-gtk)
(ql:quickload :datafly)

(defpackage :aplicacion
  (:use :gtk :gdk :gdk-pixbuf :gobject
	:glib :gio :pango :cairo :common-lisp
   :datafly :sxql))

(in-package :aplicacion)

(connect-toplevel :postgres :database-name "database" :username "username" :password "xxxx-xxxx" :host "localhost")

;;(push #'dbi:simple-sql-logger dbi:*sql-execution-hooks*)

(defun fecha-completa (valor)
  (multiple-value-bind (seconds minutes hour day month year)
      (decode-universal-time valor)
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day hour minutes seconds)))

;;--------------------------------------------------------------------
;;  Store
;;--------------------------------------------------------------------

(defclass store ()
  ((widget :initarg :widget :initform nil :accessor widget)
   (modelo :initarg :modelo :initform nil :accessor modelo)))

;;--------------------------------------------------------------------
;;  Editor Etiqueta
;;--------------------------------------------------------------------

(defclass editor-etiqueta ()
  ((widget :initarg :widget :initform nil :accessor widget)
   (nombre-etiqueta :initarg :nombre-etiqueta :initform nil :accessor nombre-etiqueta)))

(defmethod initialize-instance :after ((instance editor-etiqueta) &key)
  (let ((builder (make-instance 'gtk-builder)))
    (gtk-builder-add-from-file builder "dialogs.glade")
    (setf (widget instance) (gtk-builder-get-object builder "editor-etiquetas"))
    (setf (nombre-etiqueta instance) (gtk-builder-get-object builder "nombre-etiqueta"))))

;;--------------------------------------------------------------------
;;  Editor Nota
;;--------------------------------------------------------------------

(defclass editor-nota ()
  ((widget :initarg :widget :initform nil :accessor widget)
   (id-nota :initarg :id-nota :initform nil :accessor id-nota)
   (nombre :initarg :nombre :initform nil)
   (descripcion :initarg :descripcion :initform nil)
   (contenido :initarg :contenido :initform nil)
   (fecha :initarg :fecha :initform nil :accessor fecha)
   (toggle-renderer :initarg :toggle-renderer :initform nil :accessor toggle-renderer)
   (tipo-nota :initarg :tipo-nota :initform (make-instance 'store) :accessor tipo-nota)
   (etiquetas :initarg :etiquetas :initform (make-instance 'store) :accessor etiquetas)
   (lista-etiquetas :initarg :lista-etiquetas :initform nil :accessor lista-etiquetas)))

(defgeneric nombre (editor-nota))
(defgeneric descripcion (editor-nota))
(defgeneric contenido (editor-nota))

(defgeneric (setf nombre) (texto editor-nota))
(defgeneric (setf descripcion) (texto editor-nota))
(defgeneric (setf contenido) (texto editor-nota))

(defgeneric agregar-etiqueta (editor-nota))
(defgeneric modificar-etiqueta (editor-nota))
(defgeneric eliminar-etiqueta (editor-nota))
(defgeneric seleccionar-etiquetas (editor-nota))
(defgeneric actualizar-lista-etiquetas (editor-nota))

(defmethod nombre ((instance editor-nota)) (gtk-entry-text (slot-value instance 'nombre)))

(defmethod descripcion ((instance editor-nota)) (gtk-entry-text (slot-value instance 'descripcion)))

(defmethod contenido ((instance editor-nota))
  (let ((buffer (gtk-text-view-get-buffer (slot-value instance 'contenido)))
	start-iter end-iter)
    (setf start-iter (gtk-text-buffer-get-start-iter buffer))
    (setf end-iter (gtk-text-buffer-get-end-iter buffer))
    (gtk-text-buffer-get-text buffer start-iter end-iter t)))

(defmethod (setf nombre) (texto (instance editor-nota))
  (setf (gtk-entry-text (slot-value instance 'nombre)) texto))

(defmethod (setf descripcion) (texto (instance editor-nota))
  (setf (gtk-entry-text (slot-value instance 'descripcion)) texto))

(defmethod (setf contenido) (texto (instance editor-nota))
  (let ((buffer (gtk-text-view-get-buffer (slot-value instance 'contenido))))
    (gtk-text-buffer-set-text buffer texto)))

(defmethod agregar-etiqueta ((instance editor-nota))
  (let ((dialogo (make-instance 'editor-etiqueta)))
    (setf (gtk-window-title (widget dialogo)) "Agregar Etiqueta")
    (when (string= "OK" (gtk-dialog-run (widget dialogo)))
      ;;(gtk-widget-hide dialogo)
      (execute (insert-into :etiqueta_nota (set= :nombre (gtk-entry-text (nombre-etiqueta dialogo)))))
      (actualizar-lista-etiquetas instance)
      (seleccionar-etiquetas instance))
    (gtk-widget-destroy (widget dialogo))))

(defmethod modificar-etiqueta ((instance editor-nota))
  (let ((dialogo (make-instance 'editor-etiqueta))
	(selection (gtk-tree-view-get-selection (widget (etiquetas instance))))
	id nombre iter)
    (setf iter (gtk-tree-selection-get-selected selection))
    (setf id (gtk-tree-model-get-value (modelo (etiquetas instance)) iter 2))
    (setf nombre (gtk-tree-model-get-value (modelo (etiquetas instance)) iter 1))
    (setf (gtk-entry-text (nombre-etiqueta dialogo)) nombre)
    (when (string= "OK" (gtk-dialog-run (widget dialogo)))
      ;;(gtk-widget-hide dialogo)
      (execute (update :etiqueta_nota (set= :nombre (gtk-entry-text (nombre-etiqueta dialogo))) (where (:= :id id))))
      (actualizar-lista-etiquetas instance)
      (seleccionar-etiquetas instance))
    (gtk-widget-destroy (widget dialogo))))

(defmethod eliminar-etiqueta ((instance editor-nota))
  (let (iter selection dialogo id nombre)
    (setf selection (gtk-tree-view-get-selection (widget (etiquetas instance))))
    (setf iter (gtk-tree-selection-get-selected selection))
    (setf nombre (gtk-tree-model-get-value (modelo (etiquetas instance)) iter 1))
    (setf id (gtk-tree-model-get-value (modelo (etiquetas instance)) iter 2))
    (setf dialogo
	  (make-instance 'gtk-message-dialog
			 :message-type :question
			 :buttons :yes-no
			 :text "¿Desea eliminar la etiqueta?"
			 :secondary-text (format nil "~d - ~s" id nombre)))
    (when (string= "YES" (gtk-dialog-run dialogo))
      ;;(gtk-widget-hide dialogo)
      (execute (delete-from :etiqueta_nota (where (:= :id id))))
      (actualizar-lista-etiquetas instance)
      (seleccionar-etiquetas instance))
    (gtk-widget-destroy dialogo)))

(defmethod seleccionar-etiquetas ((instance editor-nota))
  (let (id (iter (gtk-tree-model-get-iter-first (modelo (etiquetas instance)))))
    (loop while iter
    	  do (setf id (gtk-tree-model-get-value (modelo (etiquetas instance)) iter 2))
	  (when (member id (lista-etiquetas instance))
	    (gtk-list-store-set (modelo (etiquetas instance)) iter t))
	  (setf iter (gtk-tree-model-iter-next (modelo (etiquetas instance)) iter)))))

(defmethod actualizar-lista-etiquetas ((instance editor-nota))
  (gtk-list-store-clear (modelo (etiquetas instance)))
  (loop for element in (retrieve-all (select :* (from :etiqueta_nota) (order-by :id)))
	do (gtk-list-store-set
	    (modelo (etiquetas instance))
	    (gtk-list-store-append (modelo (etiquetas instance)))
	    nil (getf element :nombre) (getf element :id))))

(defmethod initialize-instance :after ((instance editor-nota) &key)
  (let ((builder (make-instance 'gtk-builder)))
    (gtk-builder-add-from-file builder "dialogs.glade")
    ;;(gtk-builder-connect-signals-auto builder #.(find-package '#:aplicacion))
    (setf (widget instance) (gtk-builder-get-object builder "editor-notas"))
    (setf (widget (tipo-nota instance)) (gtk-builder-get-object builder "tipo-nota"))
    (setf (modelo (tipo-nota instance)) (gtk-combo-box-get-model (widget (tipo-nota instance))))
    (setf (widget (etiquetas instance)) (gtk-builder-get-object builder "nota-etiqueta"))
    (setf (modelo (etiquetas instance)) (gtk-builder-get-object builder "modelo"))
    (setf (slot-value instance 'nombre) (gtk-builder-get-object builder "nombre"))
    (setf (slot-value instance 'descripcion) (gtk-builder-get-object builder "descripcion"))
    (setf (slot-value instance 'contenido) (gtk-builder-get-object builder "contenido"))
    (g-signal-connect (slot-value instance 'nombre) "icon-press"
		      (lambda (msg icon-pos event)
			(declare (ignore msg icon-pos event))
			(setf (gtk-entry-text (slot-value instance 'nombre)) "")))
    (g-signal-connect (slot-value instance 'descripcion) "icon-press"
		      (lambda (msg icon-pos event)
			(declare (ignore msg icon-pos event))
			(setf (gtk-entry-text (slot-value instance 'descripcion)) "")))
    (g-signal-connect (gtk-builder-get-object builder "agregar-etiqueta") "clicked"
    		      (lambda (msg) (declare (ignore msg)) (agregar-etiqueta instance)))
    (g-signal-connect (gtk-builder-get-object builder "modificar-etiqueta") "clicked"
    		      (lambda (msg) (declare (ignore msg)) (modificar-etiqueta instance)))
    (g-signal-connect (gtk-builder-get-object builder "eliminar-etiqueta") "clicked"
    		      (lambda (msg) (declare (ignore msg)) (eliminar-etiqueta instance))))
  (setf (toggle-renderer instance) (make-instance 'gtk-cell-renderer-toggle))
  (g-signal-connect (toggle-renderer instance) "toggled"
  		    (lambda (msg path)
  		      (declare (ignore msg))
  		      (let (id estado valor iter (tree-path (gtk-tree-path-new-from-string path)))
  		      	(setf iter (gtk-tree-model-get-iter (modelo (etiquetas instance)) tree-path))
  		      	(setf valor (gtk-tree-model-get (modelo (etiquetas instance)) iter 0))
  		      	(setf id (gtk-tree-model-get (modelo (etiquetas instance)) iter 2))
			(setf (lista-etiquetas instance)
			      (if (not (car valor))
			      	  (append (lista-etiquetas instance) id)
			      	  (remove (car id) (lista-etiquetas instance))))
			(setf estado (not (car valor)))
  		      	(gtk-list-store-set (modelo (etiquetas instance)) iter estado))))
  (let ((columna (gtk-tree-view-column-new-with-attributes "Nombre" (toggle-renderer instance) "active" 0))
	(text-renderer (make-instance 'gtk-cell-renderer-text)))
    (gtk-tree-view-column-pack-start columna text-renderer)
    (gtk-tree-view-column-set-attributes columna text-renderer "text" 1)
    (gtk-tree-view-append-column (widget (etiquetas instance)) columna))
  (actualizar-lista-etiquetas instance)
  (loop for element in (retrieve-all (select :* (from :tipo_nota) (order-by :id)))
	do (gtk-list-store-set
	    (modelo (tipo-nota instance))
	    (gtk-list-store-append (modelo (tipo-nota instance)))
	    (getf element :nombre)))
  (gtk-combo-box-set-active (widget (tipo-nota instance)) 0)
  (gtk-tree-view-set-cursor (widget (etiquetas instance)) (gtk-tree-path-new-from-string "0"))
  (setf (gtk-window-title (widget instance)) "Editar Nota")
  (gtk-window-set-default-geometry (widget instance) 800 600))

;;--------------------------------------------------------------------
;;  Agenda
;;--------------------------------------------------------------------

(defclass agenda ()
  ((widget :initarg :widget :initform nil :accessor widget)
   (status-bar :initarg :status-bar :initform (make-instance 'store :widget (make-instance 'gtk-statusbar) :modelo 0) :accessor status-bar)
   (lista :initarg :lista :initform (make-instance 'store) :accessor lista)))

(defgeneric salir (agenda))
(defgeneric agregar-nota (agenda))
(defgeneric modificar-nota (agenda))
(defgeneric eliminar-nota (agenda))
(defgeneric realizar-consulta (agenda))
(defgeneric actualizar-lista (agenda))

(defmethod salir ((instance agenda))
  (disconnect-toplevel)
  (gtk-main-quit))

(defmethod agregar-nota ((instance agenda))
  (let ((dialogo (make-instance 'editor-nota)))
    (setf (gtk-window-title (widget dialogo)) "Agregar Nota")
    ;; * NOTA *
    ;; Todavia no podemos definir las etiquetas para la
    ;; nueva nota porque no podemos obtener el id
    ;; del nuevo registro (aun no sabemos como).
    (when (string= "OK" (gtk-dialog-run (widget dialogo)))
      ;;(gtk-widget-hide dialogo)
      (gtk-widget-hide (widget dialogo))
      (execute
       (insert-into :nota
		    (set=
		     :nombre (nombre dialogo)
		     :descripcion (descripcion dialogo)
		     :contenido (contenido dialogo)
		     :tipo_nota (+ (gtk-combo-box-get-active (widget (tipo-nota dialogo))) 1))
		    (returning :id)))
      (actualizar-lista instance))
    (gtk-widget-destroy (widget dialogo))))

(defmethod modificar-nota ((instance agenda))
  (let (id iter nota tipo-nota
	   (dialogo (make-instance 'editor-nota))
  	   (selection (gtk-tree-view-get-selection (widget (lista instance)))))
    (setf iter (gtk-tree-selection-get-selected selection))
    (setf id (gtk-tree-model-get-value (modelo (lista instance)) iter 1))
    (setf (id-nota dialogo) id)
    (setf nota (retrieve-one (select :* (from :nota) (where (:= :id id)))))
    (setf tipo-nota (retrieve-one (select :* (from :tipo_nota) (where (:= :id (getf nota :tipo-nota))))))
    (setf (nombre dialogo) (getf nota :nombre))
    (setf (descripcion dialogo) (or (getf nota :descripcion) ""))
    (setf (contenido dialogo) (getf nota :contenido))
    (let (combo-box-iter texto (indice 0))
      (setf combo-box-iter (gtk-tree-model-get-iter-first (modelo (tipo-nota dialogo))))
      (loop while combo-box-iter do
	    (setf texto (gtk-tree-model-get-value (modelo (tipo-nota dialogo)) combo-box-iter 0))
	    (when (string= texto (getf tipo-nota :nombre)) (gtk-combo-box-set-active (widget (tipo-nota dialogo)) indice))
	    (setf combo-box-iter (gtk-tree-model-iter-next (modelo (tipo-nota dialogo)) combo-box-iter))
	    (setf indice (+ indice 1)) until (string= texto (getf tipo-nota :nombre))))
    (let (guardados seleccionados)
      (loop for elemento in (retrieve-all (select :* (from :nota_etiqueta) (where (:= :nota (getf nota :id)))))
    	    do (push (getf elemento :etiqueta-nota) guardados))
      (setf (lista-etiquetas dialogo) guardados)
      (seleccionar-etiquetas dialogo)
      (setf (gtk-window-title (widget dialogo)) (format nil "Editar nota #~d" id))
      (when (string= "OK" (gtk-dialog-run (widget dialogo)))
	;;(gtk-widget-hide dialogo)
	(execute
	 (update :nota
		 (set=
		  :nombre (nombre dialogo)
		  :descripcion (descripcion dialogo)
		  :contenido (contenido dialogo)
		  :tipo_nota (+ (gtk-combo-box-get-active (widget (tipo-nota dialogo))) 1))
		 (where (:= :id (id-nota dialogo)))))
	(setf seleccionados (lista-etiquetas dialogo))
	(loop for elemento in (set-difference guardados seleccionados)
	      do (execute (delete-from :nota_etiqueta (where (:and (:= :nota (id-nota dialogo)) (:= :etiqueta_nota elemento))))))
	(loop for elemento in (set-difference seleccionados guardados)
	      do (execute (insert-into :nota_etiqueta (set= :nota (id-nota dialogo) :etiqueta_nota elemento))))
	(actualizar-lista instance))
      (gtk-widget-destroy (widget dialogo)))))

(defmethod eliminar-nota ((instance agenda))
  (let (iter selection dialogo id nombre)
    (setf selection (gtk-tree-view-get-selection (widget (lista instance))))
    (setf iter (gtk-tree-selection-get-selected selection))
    (setf id (gtk-tree-model-get-value (modelo (lista instance)) iter 1))
    (setf nombre (gtk-tree-model-get-value (modelo (lista instance)) iter 2))
    (setf dialogo (make-instance 'gtk-message-dialog
				 :message-type :question
				 :buttons :yes-no
				 :text "¿Desea eliminar la nota?"
				 :secondary-text (format nil "~s" nombre)))
    (when (string= "YES" (gtk-dialog-run dialogo))
      ;;(gtk-widget-hide dialogo)
      (format t "La nota ~d - ~s se elimina~%" id nombre))
    (gtk-widget-destroy dialogo)))

(defmethod ejecutar-consulta ((instance agenda))
  )

(defmethod actualizar-lista ((instance agenda))
  (gtk-list-store-clear (modelo (lista instance)))
  ;;(loop for elemento in (retrieve-all (select :* (from :nota)))
  ;;(loop for elemento in (retrieve-all (select (:id :nombre :descripcion :fecha :tipo_nota) (from :nota) (order-by :id)))
  ;; 5540285131
  (loop for elemento in (retrieve-all (select (fields :id :nombre :descripcion :fecha :tipo_nota) (from :nota) (order-by :id)))
	do (gtk-list-store-set
	    (modelo (lista instance))
	    (gtk-list-store-append (modelo (lista instance)))
	    nil
	    (getf elemento :id)
	    (getf elemento :nombre)
	    (or (getf elemento :descripcion) "")
	    (fecha-completa (getf elemento :fecha))
	    (flet ((predicado (tipo) (getf tipo :id)))
		  (let ((id (getf elemento :tipo-nota))
			(tipos (retrieve-all (select :* (from :tipo_nota) (order-by :id)))))
		    (getf (find id tipos :key #'predicado) :nombre)))))
  (gtk-statusbar-pop (widget (status-bar instance)) (modelo (status-bar instance)))
  (let ((msg (format nil "~d registros" (car (cdr (retrieve-one (select (fields (:count :*)) (from :nota))))))))
    (setf (modelo (status-bar instance)) (gtk-statusbar-get-context-id (widget (status-bar instance)) msg))
    (gtk-statusbar-push (widget (status-bar instance)) (modelo (status-bar instance)) msg)))

(defmethod initialize-instance :after ((instance agenda) &key)
  (let ((builder (make-instance 'gtk-builder)))
    (gtk-builder-add-from-file builder "agenda.glade")
    (setf (widget instance) (gtk-builder-get-object builder "window"))
    (setf (widget (lista instance)) (gtk-builder-get-object builder "lista"))
    (setf (modelo (lista instance)) (make-instance 'gtk-list-store :column-types '("gboolean" "guint" "gchararray" "gchararray" "gchararray" "gchararray")))
    (setf (gtk-tree-view-model (widget (lista instance))) (modelo (lista instance)))
    (g-signal-connect (widget instance) "destroy" (lambda (msg) (declare (ignore msg)) (salir instance)))
    (g-signal-connect (gtk-builder-get-object builder "salir") "clicked" (lambda (msg) (declare (ignore msg)) (salir instance)))
    (g-signal-connect (gtk-builder-get-object builder "agregar-nota") "clicked" (lambda (msg) (declare (ignore msg)) (agregar-nota instance)))
    (g-signal-connect (gtk-builder-get-object builder "modificar-nota") "clicked" (lambda (msg) (declare (ignore msg)) (modificar-nota instance)))
    (g-signal-connect (gtk-builder-get-object builder "eliminar-nota") "clicked" (lambda (msg) (declare (ignore msg)) (eliminar-nota instance)))
    (g-signal-connect (gtk-builder-get-object builder "ejecutar-consulta") "clicked" (lambda (msg) (declare (ignore msg)) (ejecutar-consulta instance)))
    (g-signal-connect (gtk-builder-get-object builder "actualizar-lista") "clicked" (lambda (msg) (declare (ignore msg)) (actualizar-lista instance)))
    (gtk-box-pack-end (gtk-builder-get-object builder "box") (widget (status-bar instance)) :expand nil :fill nil :padding 0))
  (gtk-tree-view-append-column (widget (lista instance)) (gtk-tree-view-column-new-with-attributes "" (make-instance 'gtk-cell-renderer-toggle) "active" 0))
  (gtk-tree-view-append-column (widget (lista instance)) (gtk-tree-view-column-new-with-attributes "id" (make-instance 'gtk-cell-renderer-text) "text" 1))
  (gtk-tree-view-append-column (widget (lista instance)) (gtk-tree-view-column-new-with-attributes "Nombre" (make-instance 'gtk-cell-renderer-text) "text" 2))
  (gtk-tree-view-append-column (widget (lista instance)) (gtk-tree-view-column-new-with-attributes "Descripcion" (make-instance 'gtk-cell-renderer-text) "text" 3))
  (gtk-tree-view-append-column (widget (lista instance)) (gtk-tree-view-column-new-with-attributes "Fecha" (make-instance 'gtk-cell-renderer-text) "text" 4))
  (gtk-tree-view-append-column (widget (lista instance)) (gtk-tree-view-column-new-with-attributes "Tipo de nota" (make-instance 'gtk-cell-renderer-text) "text" 5))
  (setf (modelo (status-bar instance)) (gtk-statusbar-get-context-id (widget (status-bar instance)) "0 registros"))
  (gtk-statusbar-push (widget (status-bar instance)) (modelo (status-bar instance)) "0 registros")
  (loop for i from 0 to 5
	do (let ((column (gtk-tree-view-get-column (widget (lista instance)) i)))
	     (gtk-tree-view-column-set-resizable column t)
	     (gtk-tree-view-column-set-sort-column-id column i)))
  (actualizar-lista instance)  
  (setf (gtk-window-title (widget instance)) "Agenda")
  (gtk-window-set-default-geometry (widget instance) 1024 768)
  (gtk-widget-grab-focus (widget (lista instance)))
  (gtk-tree-view-set-cursor (widget (lista instance)) (gtk-tree-path-new-from-string "0")))

(within-main-loop
  (let ((app (make-instance 'agenda)))
    (gtk-widget-show-all (widget app))))
