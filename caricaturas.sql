-- -*- mode: sql; sql-product: postgres -*-

-- id integer generated by default as identity primary key
-- id serial primary key

create type dia as enum (
    'domingo',
    'lunes',
    'martes',
    'miercoles',
    'jueves',
    'viernes',
    'sabado'
);

-- -------------------------------------------------------------------
--  anime
-- -------------------------------------------------------------------

create table anime (
    id integer generated by default as identity primary key,
    nombre text,
    direccion text,
    fecha timestamp with time zone default now()
);

create table genero_anime (
    id integer generated by default as identity primary key,
    nombre text
);

create table anime_genero (
    anime integer references anime (id) on delete cascade on update cascade,
    genero integer references genero_anime (id) on delete cascade on update cascade,
    primary key (anime, genero)
);

create table anime_emision (
    id integer generated by default as identity primary key,
    anime integer references anime (id) on delete cascade on update cascade,
    dia dia default 'domingo'::dia,
    hora time default '00:00',
    fecha timestamp with time zone default now()
);

create table detalle_anime (
    id integer generated by default as identity primary key,
    anime integer references anime (id) on delete cascade on update cascade,
    capitulos integer default 12,
    censurado boolean default false,
    calificacion integer check (calificacion >= 1 and calificacion <= 5) default 3,
    fecha timestamp with time zone default now()
);

create table lista_anime (
    id integer generated by default as identity primary key,
    nombre text,
    fecha timestamp with time zone default now()
);

create table lista_anime_anime (
    lista integer references lista_anime (id) on delete cascade on update cascade,
    anime integer references anime (id) on delete cascade on update cascade,
    fecha timestamp with time zone default now(),
    primary key (lista, anime)
);

create table comentario_anime (
    id integer generated by default as identity primary key,
    anime integer references anime (id) on delete cascade on update cascade,
    contenido text,
    fecha timestamp with time zone default now()
);

create table comentario_anime_emision (
    id integer generated by default as identity primary key,
    anime integer references anime_emision (id) on delete cascade on update cascade,
    contenido text,
    fecha timestamp with time zone default now()
);

create table comentario_lista_anime (
    id integer generated by default as identity primary key,
    lista integer references lista_anime (id) on delete cascade on update cascade,
    contenido text,
    fecha timestamp with time zone default now()
);

create table nombre_alterno_anime (
    id integer generated by default as identity primary key,
    anime integer references anime(id) on delete cascade on update cascade,
    nombre text,
    fecha timestamp with time zone default now()
);

-- -------------------------------------------------------------------
--  codigo nuclear
-- -------------------------------------------------------------------

create table codigo_nuclear (
    id integer generated by default as identity primary key,
    codigo text,
    fecha timestamp with time zone default now()
);

create table comentario_codigo_nuclear (
    id integer generated by default as identity primary key,
    codigo integer references codigo_nuclear(id) on delete cascade on update cascade,
    contenido text,
    fecha timestamp with time zone default now()
);

-- -------------------------------------------------------------------
--  Seiyuu
-- -------------------------------------------------------------------

create table actor (
    id integer generated by default as indentity primary key,
    nombre text,
    apellido text,
    --fecha_nacimiento date,
);

create table anime (
    id integer generated by default as identity primary key,
    nombre text
);

create table personaje (
    id integer generated by default as identity primary key,
    nombre text,
);

create table personajes (
    anime integer references anime (id) on delete cascade on update cascade,
    personaje integer references personaje (id) on delete cascade on update cascade,
    primary key (anime, personaje)
);

create table rol (
    actor integer references actor (id) on delete cascade on update cascade,
    personaje integer references personaje (id) on delete cascade on update cascade,
    primary key (actor, personaje)
);
