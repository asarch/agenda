#!/usr/bin/python

import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk, Gio

from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

database = 'agenda'
#engine = create_engine('postgresql://username:password@192.168.1.100/%s' % database)
engine = create_engine('postgresql://username:password@localhost/%s' % database)
#engine = create_engine('postgresql://username:password@127.0.0.1/%s' % database)
Session = sessionmaker(bind=engine)
session = Session()

from model import Nota
from model import TipoNota
from model import EtiquetaNota

class TreeViewWidget:
	def __init__(self, modelo, widget):
		self.modelo = modelo
		self.widget = widget

class EditorNotas:
	def __init__(self):
		self.builder = Gtk.Builder()
		self.builder.add_from_file("dialogs.glade")
		#self.builder.connect_signals(EditorNotas)

		self.dirty = False
		self.widget = self.builder.get_object("editor-notas")
		self.widget.set_default_size(800, 600)

		# GtkComboBox
		self.tipo_nota = self.builder.get_object("tipo-nota")
		self.tipo_nota_model = self.tipo_nota.get_model()

		for instance in session.query(TipoNota):
			self.tipo_nota_model.set(self.tipo_nota_model.append(), 0, instance.nombre)

		self.tipo_nota.set_active(0)
		self.builder.get_object("contenido").grab_focus()

		# GtkTreeView
		self.etiquetas = TreeViewWidget(self.builder.get_object("modelo"), self.builder.get_object("nota-etiqueta"))

		columna = Gtk.TreeViewColumn("Nombre")

		self.toggle_renderer = Gtk.CellRendererToggle()
		columna.pack_start(self.toggle_renderer, False)
		columna.set_attributes(self.toggle_renderer, active=0)
		self.toggle_renderer.connect("toggled", self.cambiar_estado)

		text_renderer = Gtk.CellRendererText()
		columna.pack_start(text_renderer, True)
		columna.set_attributes(text_renderer, text=1)

		self.etiquetas.widget.append_column(columna)
		self.etiquetas.modelo.clear()

		for instance in session.query(EtiquetaNota):
			etiqueta = [False, instance.nombre, instance.id]
			self.etiquetas.modelo.append(etiqueta)

	def cambiar_estado(self, widget, path):
		self.etiquetas.modelo[path][0] = not self.etiquetas.modelo[path][0]

	def cambio (self, widget):
		self.dirty = True

	def set_nombre(self, text):
		self.builder.get_object("nombre").set_text(text)
	
	def set_descripcion(self, text):
		self.builder.get_object("descripcion").set_text(text)

	def set_contenido(self, text):
		self.builder.get_object("contenido").get_buffer().set_text(text)
	
	def set_tipo_nota(self, index):
		self.tipo_nota.set_active(index - 1)

	def get_nombre(self):
		return self.builder.get_object("nombre").get_text()

	def get_descripcion(self):
		return self.builder.get_object("descripcion").get_text()

	def get_contenido(self):
		text_view = self.builder.get_object("contenido")
		text_buffer = text_view.get_buffer()
		return text_buffer.get_text(text_buffer.get_start_iter(), text_buffer.get_end_iter(), True)

	def get_tipo_nota(self):
		return self.tipo_nota.get_active() + 1

	def run(self):
		return self.widget.run()

	def destroy(self):
		return self.widget.destroy()

	def connect_signals(self, instance):
		self.builder.connect_signals(instance)

class Agenda:
	def __init__(self):
		self.builder = Gtk.Builder()
		self.builder.add_from_file("agenda.glade")
		#self.builder.connect_signals(Horchata)

		self.widget = self.builder.get_object("window")
		self.widget.set_default_size(800, 600)

		box = self.builder.get_object("box")

		self.model = Gtk.ListStore(int, str, str, str, str)
		#self.tree_view = self.builder.get_object('tree-view')
		self.tree_view = self.builder.get_object('lista')
		self.tree_view.set_model(self.model)

		self.tree_view.append_column(Gtk.TreeViewColumn("id", Gtk.CellRendererText(), text=0))
		self.tree_view.append_column(Gtk.TreeViewColumn("Nombre", Gtk.CellRendererText(), text=1))
		self.tree_view.append_column(Gtk.TreeViewColumn("Descripcion", Gtk.CellRendererText(), text=2))
		self.tree_view.append_column(Gtk.TreeViewColumn("Fecha", Gtk.CellRendererText(), text=3))
		self.tree_view.append_column(Gtk.TreeViewColumn("Tipo", Gtk.CellRendererText(), text=4))

		for i in range(0, 5):
			self.tree_view.get_column(i).set_resizable(True)
			self.tree_view.get_column(i).set_sort_column_id(i)

		self.status_bar = Gtk.Statusbar()
		box.pack_end(self.status_bar, False, True, 0)
		self.status_bar.show();

		self.update_list()

	def connect_signals(self, instance):
		self.builder.connect_signals(instance)

	def actualizar(self, widget):
		self.update_list()

	def update_list(self):
		print("Actualizando lista\n")
		self.model.clear()

		for instance in session.query(Nota):
			if instance.tipo_nota1:
				tipo_nota = instance.tipo_nota1.nombre
			else:
				tipo_nota = "<undefined>"

			nota = [
				instance.id,
				instance.nombre,
				instance.descripcion,
				u'%s' % instance.fecha,
				tipo_nota
			]

			self.model.append(nota)

		registros = "%d registros" % (session.query(Nota).count())
		context_id = self.status_bar.get_context_id(registros)
		self.status_bar.push(context_id, registros)
		self.tree_view.set_cursor(0)

	def agregar(self, widget):
		editor = EditorNotas()
		editor.connect_signals(editor)

		if (editor.run() == Gtk.ResponseType.OK):
			nota = Nota()
			nota.nombre = editor.get_nombre()
			nota.descripcion = editor.get_descripcion()
			nota.contenido = editor.get_contenido()
			nota.tipo_nota = editor.get_tipo_nota()

			session.add(nota)
			session.commit()
			self.update_list()

		editor.destroy()

	def eliminar(self, widget):
		print("Eliminar")
	
	def consulta(self, widget):
		print("Ejecutar query")
	
	def editar(self, widget):
		selection = self.tree_view.get_selection()
		model,treeiter = selection.get_selected()
		
		if treeiter != None:
			id = model[treeiter][0]
			nota = session.query(Nota).filter(Nota.id == id).one()

			editor = EditorNotas()
			editor.set_nombre(nota.nombre)
			editor.set_descripcion(nota.descripcion)
			editor.set_contenido(nota.contenido)
			editor.set_tipo_nota(nota.tipo_nota1.id)

			if (editor.run() == Gtk.ResponseType.OK):
				nota.nombre = editor.get_nombre()
				nota.descripcion = editor.get_descripcion()
				nota.contenido = editor.get_contenido()
				#nota.tipo_nota1.id = editor.get_tipo_nota()
				nota.tipo_nota1 = session.query(TipoNota).filter(TipoNota.id == editor.get_tipo_nota()).one()
				session.commit()
				self.update_list()

			editor.destroy()
	def show(self):
		return self.widget.show()

	def salir(self, widget):
		Gtk.main_quit()

window = Agenda()
window.connect_signals(window)
window.show()
Gtk.main()
