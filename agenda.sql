-- -*- mode: sql; sql-product: postgres -*-

-- id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY
-- id serial PRIMARY KEY

CREATE TYPE dia AS ENUM (
    'domingo',
    'lunes',
    'martes',
    'miercoles',
    'jueves',
    'viernes',
    'sabado'
);

CREATE TYPE mes AS ENUM (
    'enero',
    'febrero',
    'marzo',
    'abril',
    'mayo',
    'junio',
    'julio',
    'agosto',
    'septiembre',
    'octubre',
    'noviembre',
    'diciembre'
);

CREATE TYPE prioridad_tarea AS ENUM (
    'baja',
    'normal',
    'alta'
);

-- -------------------------------------------------------------------
-- Anime
-- -------------------------------------------------------------------

CREATE TABLE anime (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    direccion text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE genero_anime (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text
);

CREATE TABLE anime_genero (
    anime integer REFERENCES anime (id) ON DELETE CASCADE ON UPDATE CASCADE,
    genero integer REFERENCES genero_anime (id) ON DELETE CASCADE ON UPDATE CASCADE,
    PRIMARY KEY (anime, genero)
);

CREATE TABLE anime_emision (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anime integer REFERENCES anime (id) ON DELETE CASCADE ON UPDATE CASCADE,
    dia dia DEFAULT 'domingo'::dia,
    hora time DEFAULT '00:00',
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE anime_pendiente (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anime integer REFERENCES anime (id) ON DELETE CASCADE ON UPDATE CASCADE,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE anime_selecto (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anime integer REFERENCES anime (id) ON DELETE CASCADE ON UPDATE CASCADE,
    calificacion integer CHECK (calificacion >= 1 AND calificacion <= 5) DEFAULT 3,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE anime_viendo (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anime integer REFERENCES anime (id) ON DELETE CASCADE ON UPDATE CASCADE,
    episodio integer,
    direccion text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE anime_visto (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anime integer REFERENCES anime (id) ON DELETE CASCADE ON UPDATE CASCADE,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE servicio_streaming (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    direccion text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE detalle_anime (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anime integer REFERENCES anime (id) ON DELETE CASCADE ON UPDATE CASCADE,
    capitulos integer,
    censurado boolean DEFAULT false,
    calificacion integer CHECK (calificacion >= 1 AND calificacion <= 5) DEFAULT 3,
    servicio integer REFERENCES servicio_streaming (id) ON DELETE CASCADE ON UPDATE CASCADE,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_anime (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anime integer REFERENCES anime (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_anime_emision (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anime integer REFERENCES anime_emision (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_anime_selecto (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anime integer REFERENCES anime_selecto (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_anime_viendo (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anime integer REFERENCES anime_viendo (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Aniversario
-- -------------------------------------------------------------------

CREATE TABLE tipo_aniversario (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE aniversario (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    tipo integer REFERENCES tipo_aniversario (id) ON DELETE CASCADE ON UPDATE CASCADE,
    descripcion text,
    dia integer DEFAULT extract(DAY FROM now()),
    mes mes DEFAULT 'enero'::mes,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_aniversario (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    aniversario integer REFERENCES aniversario (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Anotacion Rapida
-- -------------------------------------------------------------------

CREATE TABLE anotacion_rapida (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    fecha timestamp WITH TIME ZONE DEFAULT now(),
    contenido text
);

CREATE TABLE comentario_anotacion_rapida (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    anotacion integer REFERENCES anotacion_rapida (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Lenguaje de Programacion
-- -------------------------------------------------------------------

CREATE TABLE lenguaje_programacion (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text
);

CREATE TABLE impresion (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    version text,
    lenguaje integer REFERENCES lenguaje_programacion (id) ON DELETE CASCADE ON UPDATE CASCADE,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Articulo
-- -------------------------------------------------------------------

CREATE TABLE articulo (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    titulo text,
    autor text,
    direccion text,
    lenguaje integer REFERENCES lenguaje_programacion (id) ON DELETE CASCADE ON UPDATE CASCADE,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_articulo (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    articulo integer REFERENCES articulo (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Codigo Fuente
-- -------------------------------------------------------------------

CREATE TABLE codigo_fuente (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    contenido text,
    lenguaje integer REFERENCES lenguaje_programacion (id) ON DELETE CASCADE ON UPDATE CASCADE,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_codigo_fuente (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    codigo integer REFERENCES codigo_fuente (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Snippet
-- -------------------------------------------------------------------

CREATE TABLE snippet (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    descripcion text,
    codigo text,
    lenguaje integer REFERENCES lenguaje_programacion (id) ON DELETE CASCADE ON UPDATE CASCADE,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Pregunta
-- -------------------------------------------------------------------

CREATE TABLE autor_pregunta (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE autor_respuesta (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE pregunta (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    autor integer,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE respuesta (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    pregunta integer,
    autor integer,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_pregunta (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    pregunta integer REFERENCES pregunta (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_respuesta (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    respuesta integer REFERENCES respuesta (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Cita
-- -------------------------------------------------------------------

CREATE TABLE cita (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    texto text,
    autor text
);

CREATE TABLE comentario_cita (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    cita integer REFERENCES cita (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Codigo Nuclear
-- -------------------------------------------------------------------

CREATE TABLE codigo_nuclear (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    codigo text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_codigo_nuclear (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    codigo integer REFERENCES codigo_nuclear (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE detalle_codigo_nuclear (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    codigo integer REFERENCES codigo_nuclear (id) ON DELETE CASCADE ON UPDATE CASCADE,
    titulo text,
    idioma integer,
    paginas integer,
    calificacion integer CHECK (calificacion >= 1 AND calificacion <= 5) DEFAULT 1,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Cuenta
-- -------------------------------------------------------------------

CREATE TABLE cuenta (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    clave text,
    descripcion text,
    activo boolean DEFAULT true,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_cuenta (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    cuenta integer REFERENCES cuenta (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Diario
-- -------------------------------------------------------------------

CREATE TABLE diario (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    evento text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_diario (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    diario integer REFERENCES diario (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Enlace
-- -------------------------------------------------------------------

CREATE TABLE enlace (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    direccion text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE lista_enlace (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE etiqueta_enlace (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text
);

CREATE TABLE enlace_etiqueta (
    enlace integer REFERENCES enlace (id) ON DELETE CASCADE ON UPDATE CASCADE,
    etiqueta integer REFERENCES etiqueta_enlace (id) ON DELETE CASCADE ON UPDATE CASCADE,
    PRIMARY KEY (enlace, etiqueta)
);

CREATE TABLE comentario_enlace (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    enlace integer REFERENCES enlace (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_lista_enlace (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    lista integer REFERENCES lista_enlace (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE lista_enlace_enlace (
    lista integer REFERENCES lista_enlace (id) ON DELETE CASCADE ON UPDATE CASCADE,
    enlace integer REFERENCES enlace (id) ON DELETE CASCADE ON UPDATE CASCADE,
    fecha timestamp WITH TIME ZONE DEFAULT now(),
    PRIMARY KEY (lista, enlace)
);

-- -------------------------------------------------------------------
-- Gasolina
-- -------------------------------------------------------------------

CREATE TABLE gasolina (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    litros numeric,
    precio numeric,
    lleno boolean DEFAULT false,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_gasolina (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    recarga integer REFERENCES gasolina (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Expresion Idiomatica
-- -------------------------------------------------------------------

CREATE TABLE idioma (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text
);

CREATE TABLE expresion_idiomatica (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    oracion text,
    significado text,
    idioma integer REFERENCES idioma (id) ON DELETE CASCADE ON UPDATE CASCADE,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE japones (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    escritura text,
    romaji text,
    traduccion text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

-- -------------------------------------------------------------------
-- Libro
-- -------------------------------------------------------------------

CREATE TABLE libro (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    titulo text,
    autor text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE parrafo_libro (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    libro integer REFERENCES libro (id) ON DELETE CASCADE ON UPDATE CASCADE,
    referencia text,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_parrafo_libro (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    parrafo integer REFERENCES parrafo_libro (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);
-- -------------------------------------------------------------------
-- Nota
-- -------------------------------------------------------------------

CREATE TABLE etiqueta_nota (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text
);

CREATE TABLE tipo_nota (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text
);

CREATE TABLE nota (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    descripcion text,
    contenido text,
    tipo integer REFERENCES tipo_nota (id) ON DELETE CASCADE ON UPDATE CASCADE,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE nota_etiqueta (
    nota integer REFERENCES nota (id) ON DELETE CASCADE ON UPDATE CASCADE,
    etiqueta integer REFERENCES etiqueta_nota (id) ON DELETE CASCADE ON UPDATE CASCADE,
    PRIMARY KEY (nota, etiqueta)
);

-- -------------------------------------------------------------------
-- Tarea
-- -------------------------------------------------------------------

CREATE TABLE tipo_tarea (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text
);

CREATE TABLE tarea (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    tipo integer REFERENCES tipo_tarea (id) ON DELETE CASCADE ON UPDATE CASCADE,
    descripcion text,
    prioridad prioridad_tarea DEFAULT 'normal'::prioridad_tarea,
    concluido boolean DEFAULT false,
    realizado timestamp WITH TIME ZONE,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE comentario_tarea (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    tarea integer REFERENCES tarea (id) ON DELETE CASCADE ON UPDATE CASCADE,
    contenido text,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);
