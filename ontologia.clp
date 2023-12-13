(defclass Escritor
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot ha_escrito
        (type INSTANCE)
        (create-accessor read-write))
    (slot año_de_nacimiento
        (type INTEGER)
        (create-accessor read-write))
    (slot fama
        (type STRING)
        (create-accessor read-write))
    (slot nacionalidad
        (type STRING)
        (create-accessor read-write))
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Lector
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot se_le_recomienda
        (type INSTANCE)
        (create-accessor read-write))
    (slot autores_favoritos
        (type STRING)
        (create-accessor read-write))
    (slot edad
        (type INTEGER)
        (create-accessor read-write))
    (multislot epocas_preferidas
        (type STRING)
        (create-accessor read-write))
    (slot estado_animico_deseado
        (type STRING)
        (create-accessor read-write))
    (multislot formato_preferido
        (type STRING)
        (create-accessor read-write))
    (slot horas_lectura_semanales
        (type STRING)
        (create-accessor read-write))
    (multislot idiomas_preferidos
        (type STRING)
        (create-accessor read-write))
    (slot lugares_lectura
        (type STRING)
        (create-accessor read-write))
    (slot nivel_de_lectura
        (type STRING)
        (create-accessor read-write))
    (multislot subgeneros_preferidos
        (type STRING)
        (create-accessor read-write))
)

(defclass Novela
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot escrito_por
        (type INSTANCE)
        (create-accessor read-write))
    (slot año_publicacion
        (type INTEGER)
        (create-accessor read-write))
    (slot epoca
        (type STRING)
        (create-accessor read-write))
    (slot exito
        (type STRING)
        (create-accessor read-write))
    (slot extension
        (type STRING)
        (create-accessor read-write))
    (multislot formato_libro
        (type STRING)
        (create-accessor read-write))
    (multislot idiomas
        (type STRING)
        (create-accessor read-write))
    (multislot publico_dirigido
        (type STRING)
        (create-accessor read-write))
    (multislot subgenero
        (type STRING)
        (create-accessor read-write))
    (slot titulo
        (type STRING)
        (create-accessor read-write))
    (slot valoracion
        (type STRING)
        (create-accessor read-write))
)