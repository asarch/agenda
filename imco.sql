-- -*- mode: sql; sql-product: postgres -*-
-- id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY
-- fecha timestamp WITH TIME ZONE DEFAULT now()

CREATE TYPE hora AS ENUM ('08:00', '11:00', '14:00', '17:00');

CREATE TABLE alumno (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    nombre text,
    apellido_paterno text,
    apellido_materno text,
    clase hora,
    fecha timestamp WITH TIME ZONE DEFAULT now()
);

CREATE TABLE clase (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    hora hora,
    fecha date DEFAULT current_date
);

CREATE TABLE asistencia (
    clase integer REFERENCES clase (id) ON DELETE CASCADE ON UPDATE CASCADE,
    alumno integer REFERENCES alumno (id) ON DELETE CASCADE ON UPDATE CASCADE,
    PRIMARY KEY (clase, alumno)
);

CREATE TABLE evaluacion (
    id integer GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    clase integer REFERENCES clase (id) ON DELETE CASCADE ON UPDATE CASCADE,
    alumno integer REFERENCES alumno (id) ON DELETE CASCADE ON UPDATE CASCADE,
    gramatica integer,
    vocabulario integer,
    lectura integer,
    escrito integer,
    -- total integer,
);
