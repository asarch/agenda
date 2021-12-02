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

create table anime_pendiente (
    id integer generated by default as identity primary key,
    anime integer references anime (id) on delete cascade on update cascade,
    fecha timestamp with time zone default now()
);

create table anime_selecto (
    id integer generated by default as identity primary key,
    anime integer references anime (id) on delete cascade on update cascade,
    calificacion integer check (calificacion >= 1 and calificacion <= 5) default 3,
    fecha timestamp with time zone default now()
);

create table anime_viendo (
    id integer generated by default as identity primary key,
    anime integer references anime (id) on delete cascade on update cascade,
    direccion text,
    fecha timestamp with time zone default now()
);

create table anime_visto (
    id integer generated by default as identity primary key,
    anime integer references anime (id) on delete cascade on update cascade,
    fecha timestamp with time zone default now()
);

create table servicio_streaming (
    id integer generated by default as identity primary key,
    nombre text,
    direccion text,
    fecha timestamp with time zone default now()
);

create table servicio_anime (
    servicio integer references servicio_streaming (id) on delete cascade on update cascade,
    anime integer references anime (id) on delete cascade on update cascade,
    fecha timestamp with time zone default now(),
    primary key (servicio, anime)
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

create table comentario_anime_selecto (
    id integer generated by default as identity primary key,
    anime integer references anime_selecto (id) on delete cascade on update cascade,
    contenido text,
    fecha timestamp with time zone default now()
);

create table comentario_anime_viendo (
    id integer generated by default as identity primary key,
    anime integer references anime_viendo (id) on delete cascade on update cascade,
    contenido text,
    fecha timestamp with time zone default now()
);

create table comentario_detalle_anime (
    id integer generated by default as identity primary key,
    detalle integer references detalle_anime (id) on delete cascade on update cascade,
    contenido text,
    fecha timestamp with time zone default now()
);

create table comentario_lista_anime (
    id integer generated by default as identity primary key,
    lista integer references lista_anime (id) on delete cascade on update cascade,
    contenido text,
    fecha timestamp with time zone default now()
);

-- -------------------------------------------------------------------
--  codigo nuclear
-- -------------------------------------------------------------------

create table servidor_codigo_nuclear (
    id integer generated by default as identity primary key,
    nombre text,
    direccion text,
    fecha timestamp with time zone default now()
);

create table codigo_nuclear (
    id integer generated by default as identity primary key,
    codigo text,
    servidor integer references servidor_codigo_nuclear (id) on delete cascade on update cascade,
    fecha timestamp with time zone default now()
);

create table comentario_codigo_nuclear (
    id integer generated by default as identity primary key,
    codigo integer references codigo_nuclear (id) on delete cascade on update cascade,
    contenido text,
    fecha timestamp with time zone default now()
);

create table detalle_codigo_nuclear (
    id integer generated by default as identity primary key,
    codigo integer references codigo_nuclear (id) on delete cascade on update cascade,
    titulo text,
    idioma integer,
    paginas integer,
    calificacion integer check (calificacion >= 1 and calificacion <= 5) default 1,
    fecha timestamp with time zone default now()
);
