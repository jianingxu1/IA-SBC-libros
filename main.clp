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
    (slot habito_lectura
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
    (slot subgeneros_preferidos
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

(definstances instances
    
    ;; NOVELAS

    ([Harry_Potter_y_la_Piedra_Filosofal] of Novela
         (escrito_por  [J.K_Rowling])
         (año_publicacion  1997)
         (epoca  "edad_contemporanea")
         (exito  "best_seller")
         (extension  "media")
         (formato_libro  "audiolibro" "formato_digital" "texto" "texto_imagenes")
         (idiomas  "universal")
         (publico_dirigido  "para_todos")
         (subgenero  "fantasia")
         (titulo  "Harry Potter y la Piedra Filosofal")
         (valoracion  "excelente"))
    
    ([Don_Quijote] of Novela
         (escrito_por  [Miguel_de_Cervantes])
         (año_publicacion  1605)
         (epoca  "edad_moderna")
         (exito  "obras_maestras")
         (extension  "larga")
         (formato_libro  "texto" "formato_digital" "audiolibro")
         (idiomas  "castellano")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa" "aventura")
         (titulo  "Don Quijote")
         (valoracion  "excelente"))

    ([Crimen_y_castigo] of Novela
         (escrito_por  [Fiódor_Dostoievski])
         (año_publicacion  1866)
         (epoca  "edad_moderna")
         (exito  "obras_maestras")
         (extension  "media")
         (formato_libro  "texto")
         (idiomas  "ruso")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa" "policiaca")
         (titulo  "Crimen y castigo")
         (valoracion  "excelente"))

    ([Orgullo_y_prejuicio] of Novela
         (escrito_por  [Jane_Austen])
         (año_publicacion  1813)
         (epoca  "edad_moderna")
         (exito  "obras_maestras")
         (extension  "media")
         (formato_libro  "texto" "formato_digital")
         (idiomas  "inglés")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa" "romantica")
         (titulo  "Orgullo y prejuicio")
         (valoracion  "excelente"))

    ([Cien_años_de_soledad] of Novela
         (escrito_por  [Gabriel_Garcia_Marquez])
         (año_publicacion  1967)
         (epoca  "edad_contemporanea")
         (exito  "obras_maestras")
         (extension  "larga")
         (formato_libro  "texto" "formato_digital")
         (idiomas  "español")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa" "fantasia")
         (titulo  "Cien años de soledad")
         (valoracion  "excelente"))

    ([Ulises] of Novela
         (escrito_por  [James_Joyce])
         (año_publicacion  1922)
         (epoca  "edad_contemporanea")
         (exito  "obras_maestras")
         (extension  "larga")
         (formato_libro  "texto" "audiolibro")
         (idiomas  "inglés")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa")
         (titulo  "Ulises")
         (valoracion  "excelente"))

    ([1984] of Novela
         (escrito_por  [George_Orwell])
         (año_publicacion  1949)
         (epoca  "edad_contemporanea")
         (exito  "obras_maestras")
         (extension  "media")
         (formato_libro  "texto")
         (idiomas  "inglés")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa" "ciencia_ficcion")
         (titulo  "1984")
         (valoracion  "excelente"))

    ([Crimen_en_el_expreso_de_Oriente] of Novela
         (escrito_por  [Agatha_Christie])
         (año_publicacion  1934)
         (epoca  "edad_contemporanea")
         (exito  "obras_maestras")
         (extension  "media")
         (formato_libro  "texto")
         (idiomas  "inglés")
         (publico_dirigido  "adulto")
         (subgenero  "policiaca")
         (titulo  "Crimen en el expreso de Oriente")
         (valoracion  "excelente"))

    ([En_el_camino] of Novela
         (escrito_por  [Jack_Kerouac])
         (año_publicacion  1957)
         (epoca  "edad_contemporanea")
         (exito  "best_seller")
         (extension  "media")
         (formato_libro  "texto" "audiolibro" "texto_imagenes")
         (idiomas  "inglés")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa" "aventura")
         (titulo  "En el camino")
         (valoracion  "excelente"))

    ([Anna_Karenina] of Novela
         (escrito_por  [León_Tolstói])
         (año_publicacion  1877)
         (epoca  "edad_moderna")
         (exito  "obras_maestras")
         (extension  "larga")
         (formato_libro  "texto")
         (idiomas  "ruso")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa" "romantica")
         (titulo  "Anna Karenina")
         (valoracion  "excelente"))

    ([Matar_a_un_ruisenor] of Novela
         (escrito_por  [Harper_Lee])
         (año_publicacion  1960)
         (epoca  "edad_contemporanea")
         (exito  "obras_maestras")
         (extension  "media")
         (formato_libro  "texto" "texto_imagenes")
         (idiomas  "inglés")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa")
         (titulo  "Matar a un ruiseñor")
         (valoracion  "excelente"))

    ([El_gran_Gatsby] of Novela
         (escrito_por  [F._Scott_Fitzgerald])
         (año_publicacion  1925)
         (epoca  "edad_contemporanea")
         (exito  "obras_maestras")
         (extension  "media")
         (formato_libro  "texto")
         (idiomas  "inglés")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa" "romantica")
         (titulo  "El gran Gatsby")
         (valoracion  "excelente"))

    ([El_extranjero] of Novela
         (escrito_por  [Albert_Camus])
         (año_publicacion  1942)
         (epoca  "edad_contemporanea")
         (exito  "obras_maestras")
         (extension  "media")
         (formato_libro  "texto" "formato_digital")
         (idiomas  "francés")
         (publico_dirigido  "adulto" "formato_digital")
         (subgenero  "narrativa")
         (titulo  "El extranjero")
         (valoracion  "excelente"))

    ([Lo_que_el_viento_se_llevo] of Novela
         (escrito_por  [Margaret_Mitchell])
         (año_publicacion  1936)
         (epoca  "edad_contemporanea")
         (exito  "best_seller")
         (extension  "larga")
         (formato_libro  "texto" "audiolibro")
         (idiomas  "inglés")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa" "romantica" "historica")
         (titulo  "Lo que el viento se llevó")
         (valoracion  "excelente"))

    ([El_catcher_en_el_centeno] of Novela
         (escrito_por  [J.D._Salinger])
         (año_publicacion  1951)
         (epoca  "edad_contemporanea")
         (exito  "best_seller")
         (extension  "media")
         (formato_libro  "texto")
         (idiomas  "inglés")
         (publico_dirigido  "adolescente" "adulto")
         (subgenero  "narrativa")
         (titulo  "El catcher en el centeno")
         (valoracion  "excelente"))

    ([El_señor_de_los_anillos] of Novela
         (escrito_por  [J.R.R._Tolkien])
         (año_publicacion  1954)
         (epoca  "edad_contemporanea")
         (exito  "best_seller")
         (extension  "larga")
         (formato_libro  "texto")
         (idiomas  "inglés")
         (publico_dirigido  "adulto")
         (subgenero  "narrativa" "fantasia")
         (titulo  "El señor de los anillos")
         (valoracion  "excelente"))


    ([El_susurro_de_la_sombra] of Novela
         (escrito_por  [Ana_Martinez])
         (año_publicacion  2020)
         (epoca  "edad_contemporanea")
         (exito  "obras_maestras")
         (extension  "larga")
         (formato_libro  "texto_imagenes" "formato_digital")
         (idiomas  "ingles")
         (publico_dirigido  "adulto")
         (subgenero  "ciencia_ficcion" "aventura")
         (titulo  "El susurro de la sombra")
         (valoracion  "excelente"))

    ([La_senda_del_tiempo] of Novela
         (escrito_por  [Marta_Lopez])
         (año_publicacion  2015)
         (epoca  "edad_moderna")
         (exito  "best_seller")
         (extension  "media")
         (formato_libro  "formato_digital" "texto" "audiolibro")
         (idiomas  "castellano")
         (publico_dirigido  "adolescente")
         (subgenero  "fantasia" "historica")
         (titulo  "La senda del tiempo")
         (valoracion  "buena"))

    ([La_maldicion_del_reloj] of Novela
         (escrito_por  [Carlos_Ruiz])
         (año_publicacion  2018)
         (epoca  "edad_antigua")
         (exito  "libro_no_exitoso")
         (extension  "corta")
         (formato_libro  "texto")
         (idiomas  "frances" "castellano")
         (publico_dirigido  "infantil")
         (subgenero  "historica")
         (titulo  "La maldicion del reloj")
         (valoracion  "regular"))

    ([Las_sombras_del_pasado] of Novela
         (escrito_por  [Elena_Diaz])
         (año_publicacion  2016)
         (epoca  "edad_media")
         (exito  "libro_no_exitoso")
         (extension  "media")
         (formato_libro  "audiolibro" "texto_imagenes")
         (idiomas  "castellano")
         (publico_dirigido  "experimentado")
         (subgenero  "policiaca")
         (titulo  "Las sombras del pasado")
         (valoracion  "buena"))

    ([El_susurro_del_viento] of Novela
         (escrito_por  [Luis_Mendoza])
         (año_publicacion  2019)
         (epoca  "prehistoria")
         (exito  "best_seller")
         (extension  "larga")
         (formato_libro  "texto" "texto_imagenes")
         (idiomas  "universal" "ingles")
         (publico_dirigido  "para_todos")
         (subgenero  "aventura" "fantasia")
         (titulo  "El susurro del viento")
         (valoracion  "excelente"))

    ([La_sombra_del_desierto] of Novela
         (escrito_por  [Pedro_Rodriguez])
         (año_publicacion  2017)
         (epoca  "edad_contemporanea")
         (exito  "libro_no_exitoso")
         (extension  "corta")
         (formato_libro  "texto")
         (idiomas  "frances")
         (publico_dirigido  "infantil")
         (subgenero  "aventura")
         (titulo  "La sombra del desierto")
         (valoracion  "buena"))

    ([El_secreto_del_bosque] of Novela
         (escrito_por  [Isabel_Martinez])
         (año_publicacion  2021)
         (epoca  "prehistoria")
         (exito  "best_seller")
         (extension  "larga")
         (formato_libro  "formato_digital")
         (idiomas  "castellano" "ingles")
         (publico_dirigido  "adulto")
         (subgenero  "fantasia" "historica")
         (titulo  "El secreto del bosque")
         (valoracion  "excelente"))

    ([El_enigma_de_la_luna] of Novela
         (escrito_por  [Maria_Gomez])
         (año_publicacion  2014)
         (epoca  "edad_moderna")
         (exito  "obras_maestras")
         (extension  "media")
         (formato_libro  "audiolibro")
         (idiomas  "universal" "frances")
         (publico_dirigido  "adolescente")
         (subgenero  "ciencia_ficcion" "terror")
         (titulo  "El enigma de la luna")
         (valoracion  "regular"))

    ([Las_cadenas_del_tiempo] of Novela
         (escrito_por  [Juan_Sanchez])
         (año_publicacion  2016)
         (epoca  "edad_media")
         (exito  "best_seller")
         (extension  "corta")
         (formato_libro  "texto_imagenes")
         (idiomas  "castellano" "ingles")
         (publico_dirigido  "experimentado")
         (subgenero  "policiaca" "historica")
         (titulo  "Las cadenas del tiempo")
         (valoracion  "excelente"))

    ([La_puerta_al_inframundo] of Novela
         (escrito_por  [Laura_Fernandez])
         (año_publicacion  2018)
         (epoca  "edad_antigua")
         (exito  "libro_no_exitoso")
         (extension  "media")
         (formato_libro  "formato_digital" "texto_imagenes")
         (idiomas  "castellano")
         (publico_dirigido  "para_todos")
         (subgenero  "fantasia" "terror")
         (titulo  "La puerta al inframundo")
         (valoracion  "deficiente"))

    ([La_busqueda_del_tesoro] of Novela
         (escrito_por  [Carlos_Diaz])
         (año_publicacion  2019)
         (epoca  "prehistoria")
         (exito  "libro_no_exitoso")
         (extension  "larga")
         (formato_libro  "audiolibro" "formato_digital")
         (idiomas  "ingles")
         (publico_dirigido  "infantil")
         (subgenero  "aventura" "historica")
         (titulo  "La búsqueda del tesoro")
         (valoracion  "regular"))

    ([El_susurro_del_espacio] of Novela
         (escrito_por  [Ana_Ruiz])
         (año_publicacion  2020)
         (epoca  "edad_contemporanea")
         (exito  "obras_maestras")
         (extension  "corta")
         (formato_libro  "texto_imagenes")
         (idiomas  "frances" "castellano")
         (publico_dirigido  "adolescente")
         (subgenero  "ciencia_ficcion" "aventura")
         (titulo  "El susurro del espacio")
         (valoracion  "excelente"))

    ([La_isla_de_los_secretos] of Novela
         (escrito_por  [Luisa_Martin])
         (año_publicacion  2017)
         (epoca  "edad_moderna")
         (exito  "libro_no_exitoso")
         (extension  "media")
         (formato_libro  "texto")
         (idiomas  "castellano")
         (publico_dirigido  "adulto")
         (subgenero  "historica" "aventura")
         (titulo  "La isla de los secretos")
         (valoracion  "buena"))

    ([El_misterio_del_arte] of Novela
        (escrito_por  [Eva_Sanchez])
        (año_publicacion  2018)
        (epoca  "edad_antigua")
        (exito  "obras_maestras")
        (extension  "larga")
        (formato_libro  "audiolibro" "texto")
        (idiomas  "castellano" "frances")
        (publico_dirigido  "para_todos")
        (subgenero  "historica" "fantasia")
        (titulo  "El misterio del arte")
        (valoracion  "excelente"))

    ([El_enigma_del_espacio] of Novela
         (escrito_por  [Ana_Martinez])
         (año_publicacion  2017)
         (epoca  "edad_contemporanea")
         (exito  "libro_no_exitoso")
         (extension  "corta")
         (formato_libro  "texto")
         (idiomas  "frances")
         (publico_dirigido  "infantil")
         (subgenero  "aventura")
         (titulo  "El enigma del espacio")
         (valoracion  "buena"))
    
    ([El_secreto_del_bosque_encantado] of Novela
         (escrito_por  [Marta_Lopez])
         (año_publicacion  2021)
         (epoca  "prehistoria")
         (exito  "best_seller")
         (extension  "larga")
         (formato_libro  "formato_digital")
         (idiomas  "castellano" "ingles")
         (publico_dirigido  "adulto")
         (subgenero  "fantasia" "historica")
         (titulo  "El secreto del bosque encantado")
         (valoracion  "excelente"))

    ([La_busqueda_del_tesoro_oculto] of Novela
         (escrito_por  [Maria_Gomez])
         (año_publicacion  2014)
         (epoca  "edad_moderna")
         (exito  "obras_maestras")
         (extension  "media")
         (formato_libro  "audiolibro")
         (idiomas  "universal" "frances")
         (publico_dirigido  "adolescente")
         (subgenero  "ciencia_ficcion" "terror")
         (titulo  "La búsqueda del tesoro oculto")
         (valoracion  "regular"))

    ([Las_cadenas_del_tiempo_perdido] of Novela
         (escrito_por  [Juan_Sanchez])
         (año_publicacion  2016)
         (epoca  "edad_media")
         (exito  "best_seller")
         (extension  "corta")
         (formato_libro  "texto_imagenes")
         (idiomas  "castellano" "ingles")
         (publico_dirigido  "experimentado")
         (subgenero  "policiaca" "historica")
         (titulo  "Las cadenas del tiempo perdido")
         (valoracion  "excelente"))

    ([La_puerta_al_inframundo_perdido] of Novela
         (escrito_por  [Laura_Fernandez])
         (año_publicacion  2018)
         (epoca  "edad_antigua")
         (exito  "libro_no_exitoso")
         (extension  "media")
         (formato_libro  "formato_digital" "texto_imagenes")
         (idiomas  "castellano")
         (publico_dirigido  "para_todos")
         (subgenero  "fantasia" "terror")
         (titulo  "La puerta al inframundo perdido")
         (valoracion  "deficiente"))

    ([La_isla_de_los_secretos_perdidos] of Novela
         (escrito_por  [Carlos_Diaz])
         (año_publicacion  2019)
         (epoca  "prehistoria")
         (exito  "libro_no_exitoso")
         (extension  "larga")
         (formato_libro  "audiolibro" "formato_digital")
         (idiomas  "ingles")
         (publico_dirigido  "infantil")
         (subgenero  "aventura" "historica")
         (titulo  "La isla de los secretos perdidos")
         (valoracion  "regular"))

    ([El_susurro_del_espacio_perdido] of Novela
         (escrito_por  [Ana_Ruiz])
         (año_publicacion  2020)
         (epoca  "edad_contemporanea")
         (exito  "obras_maestras")
         (extension  "corta")
         (formato_libro  "texto_imagenes")
         (idiomas  "frances" "castellano")
         (publico_dirigido  "adolescente")
         (subgenero  "ciencia_ficcion" "aventura")
         (titulo  "El susurro del espacio perdido")
         (valoracion  "excelente"))

    ([La_isla_de_los_secretos_encontrados] of Novela
         (escrito_por  [Luisa_Martin])
         (año_publicacion  2017)
         (epoca  "edad_moderna")
         (exito  "libro_no_exitoso")
         (extension  "media")
         (formato_libro  "texto")
         (idiomas  "castellano")
         (publico_dirigido  "adulto")
         (subgenero  "historica" "aventura")
         (titulo  "La isla de los secretos encontrados")
         (valoracion  "buena"))

    ([La_sombra_del_pasado_encontrada] of Novela
         (escrito_por  [Javier_Martinez])
         (año_publicacion  2015)
         (epoca  "edad_media")
         (exito  "best_seller")
         (extension  "corta")
         (formato_libro  "formato_digital" "texto_imagenes")
         (idiomas  "ingles")
         (publico_dirigido  "experimentado")
         (subgenero  "policiaca" "terror")
         (titulo  "La sombra del pasado encontrada")
         (valoracion  "excelente"))

    ([El_misterio_del_arte_encontrado] of Novela
        (escrito_por  [Eva_Sanchez])
        (año_publicacion  2018)
        (epoca  "edad_antigua")
        (exito  "obras_maestras")
        (extension  "larga")
        (formato_libro  "audiolibro" "texto")
        (idiomas  "castellano" "frances")
        (publico_dirigido  "para_todos")
        (subgenero  "historica" "fantasia")
        (titulo  "El misterio del arte encontrado")
        (valoracion "buena"))

    ;; ESCRITORES
    
    ([J.K_Rowling] of Escritor
         (ha_escrito  [Harry_Potter_y_la_Piedra_Filosofal])
         (año_de_nacimiento  1965)
         (fama  "celebridad")
         (nacionalidad  "europeo")
         (nombre  "J.K Rowling")
    )

    ([Miguel_de_Cervantes] of Escritor
         (ha_escrito  [Don_Quijote])
         (año_de_nacimiento  1000)
         (fama  "celebridad")
         (nacionalidad  "europeo")
         (nombre  "Miguel de Cervantes"))

    ([Fiódor_Dostoievski] of Escritor
         (ha_escrito  [Crimen_y_Castigo])
         (año_de_nacimiento  1821)
         (fama  "famoso")
         (nacionalidad  "europeo")
         (nombre  "Fiódor Dostoievski"))

    ([Jane_Austen] of Escritor
         (ha_escrito  [Orgullo_y_Prejuicio])
         (año_de_nacimiento  1775)
         (fama  "famoso")
         (nacionalidad  "europeo")
         (nombre  "Jane Austen"))

    ([Gabriel_Garcia_Marquez] of Escritor
         (ha_escrito  [Cien_anos_de_soledad])
         (año_de_nacimiento  1927)
         (fama  "celebridad")
         (nacionalidad  "sud_americano")
         (nombre  "Gabriel García Márquez"))

    ([James_Joyce] of Escritor
         (ha_escrito  [Ulises])
         (año_de_nacimiento  1882)
         (fama  "famoso")
         (nacionalidad  "europeo")
         (nombre  "James Joyce"))

    ([George_Orwell] of Escritor
         (ha_escrito  [1984])
         (año_de_nacimiento  1903)
         (fama  "famoso")
         (nacionalidad  "europeo")
         (nombre  "George Orwell"))
    
    ([Agatha_Christie] of Escritor
         (ha_escrito  [Crimen_en_el_Expreso_de_Oriente])
         (año_de_nacimiento  1000)
         (fama  "celebridad")
         (nacionalidad  "europeo")
         (nombre  "Agatha Christie"))
    
    ([Jack_Kerouac] of Escritor
         (ha_escrito  [En_el_camino])
         (año_de_nacimiento  1000)
         (fama  "celebridad")
         (nacionalidad  "europeo")
         (nombre  "Jack Kerouac"))

    ([León_Tolstói] of Escritor
         (ha_escrito  [Anna_Karenina])
         (año_de_nacimiento  1000)
         (fama  "celebridad")
         (nacionalidad  "asiatico")
         (nombre  "León Tolstói"))
    
    ([Harper_Lee] of Escritor
         (ha_escrito  [Matar_a_un_ruisenor])
         (año_de_nacimiento  1000)
         (fama  "famoso")
         (nacionalidad  "asiatico")
         (nombre  "Harper Lee"))
    
    ([F._Scott_Fitzgerald] of Escritor
         (ha_escrito  [El_gran_Gatsby])
         (año_de_nacimiento  1896)
         (fama  "famoso")
         (nacionalidad  "norte_americano")
         (nombre  "F. Scott Fitzgerald"))

    ([Albert_Camus] of Escritor
         (ha_escrito  [El_extranjero])
         (año_de_nacimiento  1913)
         (fama  "celebridad")
         (nacionalidad  "europeo")
         (nombre  "Albert Camus"))

    ([J.D._Salinger] of Escritor
         (ha_escrito  [El_catcher_en_el_centeno])
         (año_de_nacimiento  1919)
         (fama  "famoso")
         (nacionalidad  "norte_americano")
         (nombre  "J.D. Salinger"))

    ([Margaret_Mitchell] of Escritor
         (ha_escrito  [Lo_que_el_viento_se_llevo])
         (año_de_nacimiento  1900)
         (fama  "celebridad")
         (nacionalidad  "norte_americano")
         (nombre  "Margaret Mitchell")
    )

    ([J.R.R._Tolkien] of Escritor
         (ha_escrito  [El_señor_de_los_anillos])
         (año_de_nacimiento  1892)
         (fama  "famoso")
         (nacionalidad  "europeo")
         (nombre  "J.R.R. Tolkien")
    )

    ([Ana_Martinez] of Escritor
         (ha_escrito  [El_susurro_de_la_sombra] [El_enigma_del_espacio])
         (año_de_nacimiento  1985)
         (fama  "emergente")
         (nacionalidad  "sud_americano")
         (nombre  "Ana Martínez"))

    ([Marta_Lopez] of Escritor
         (ha_escrito  [La_senda_del_tiempo] [La_senda_del_tiempo])
         (año_de_nacimiento  1978)
         (fama  "emergente")
         (nacionalidad  "norte_americano")
         (nombre  "Marta López"))

    ([Carlos_Ruiz] of Escritor
         (ha_escrito  [La_maldicion_del_reloj])
         (año_de_nacimiento  1980)
         (fama  "emergente")
         (nacionalidad  "asiatico")
         (nombre  "Carlos Ruiz"))

    ([Elena_Diaz] of Escritor
         (ha_escrito  [Las_sombras_del_pasado])
         (año_de_nacimiento  1992)
         (fama  "no_conocido")
         (nacionalidad  "no_conocido")
         (nombre  "Elena Díaz"))

    ([Luis_Mendoza] of Escritor
         (ha_escrito  [El_susurro_del_viento])
         (año_de_nacimiento  1975)
         (fama  "celebridad")
         (nacionalidad  "sud_americano")
         (nombre  "Luis Mendoza"))

    ([Pedro_Rodriguez] of Escritor
         (ha_escrito  [La_sombra_del_desierto])
         (año_de_nacimiento  1988)
         (fama  "emergente")
         (nacionalidad  "norte_americano")
         (nombre  "Pedro Rodríguez"))

    ([Isabel_Martinez] of Escritor
         (ha_escrito  [El_secreto_del_bosque])
         (año_de_nacimiento  1990)
         (fama  "no_conocido")
         (nacionalidad  "no_conocido")
         (nombre  "Isabel Martínez"))

    ([Maria_Gomez] of Escritor
         (ha_escrito  [El_enigma_de_la_luna] [La_busqueda_del_tesoro_oculto])
         (año_de_nacimiento  1985)
         (fama  "emergente")
         (nacionalidad  "europeo")
         (nombre  "Maria Gómez"))

    ([Juan_Sanchez] of Escritor
         (ha_escrito  [Las_cadenas_del_tiempo] [Las_cadenas_del_tiempo_perdido])
         (año_de_nacimiento  1972)
         (fama  "no_conocido")
         (nacionalidad  "europeo")
         (nombre  "Juan Sánchez"))

    ([Laura_Fernandez] of Escritor
         (ha_escrito  [La_puerta_al_inframundo] [La_puerta_al_inframundo_perdido])
         (año_de_nacimiento  1987)
         (fama  "no_conocido")
         (nacionalidad  "sud_americano")
         (nombre  "Laura Fernández"))

    ([Carlos_Diaz] of Escritor
         (ha_escrito  [La_busqueda_del_tesoro] [La_isla_de_los_secretos_perdidos])
         (año_de_nacimiento  1995)
         (fama  "emergente")
         (nacionalidad  "europeo")
         (nombre  "Carlos Díaz"))

    ([Ana_Ruiz] of Escritor
         (ha_escrito  [El_susurro_del_espacio] [El_susurro_del_espacio_perdido])
         (año_de_nacimiento  1983)
         (fama  "no_conocido")
         (nacionalidad  "europeo")
         (nombre  "Ana Ruiz"))

    ([Luisa_Martin] of Escritor
         (ha_escrito  [La_isla_de_los_secretos] [La_isla_de_los_secretos_encontrados])
         (año_de_nacimiento  1990)
         (fama  "emergente")
         (nacionalidad  "norte_americano")
         (nombre  "Luisa Martín"))

    ([Eva_Sanchez] of Escritor
         (ha_escrito  [El_misterio_del_arte] [El_misterio_del_arte_encontrado])
         (año_de_nacimiento  1988)
         (fama  "no_conocido")
         (nacionalidad  "sud_americano")
         (nombre  "Eva Sánchez"))

    ([Javier_Martinez] of Escritor
         (ha_escrito  [La_sombra_del_pasado_encontrada])
         (año_de_nacimiento  1982)
         (fama  "no_conocido")
         (nacionalidad  "sud_americano")
         (nombre  "Javier Martínez")
    )

)

;; VARIABLES GLOBALES
(defglobal ?*libros* = (create$ ""))
(defglobal ?*copia_libros* = (create$ "")) ;; en caso de que llegue alguna regla 

;; MODULOS
(defmodule MAIN (export ?ALL))

;; Recoge los datos del lector (problema concreto)
(defmodule RECOGER_DATOS
   (import MAIN ?ALL)
   (export ?ALL)
)

;; Transforma el problema concreto en problema abstracto
(defmodule ABSTRAER_DATOS
   (import MAIN ?ALL)
   (import RECOGER_DATOS ?ALL)
   (export ?ALL)
)

;; Genera los libros a recomendar
(defmodule PROCESAR_DATOS
   (import MAIN ?ALL)
   (import RECOGER_DATOS ?ALL)
   (import ABSTRAER_DATOS ?ALL)
   (export ?ALL)
)

;; Muestra los libros recomendados
(defmodule MOSTRAR_LIBROS
   (import MAIN ?ALL)
   (import RECOGER_DATOS ?ALL)
   (import ABSTRAER_DATOS ?ALL)
   (import PROCESAR_DATOS ?ALL)
   (export ?ALL)
)

;; MODULO MAIN
(defrule MAIN::iniciar "Iniciar"
   (declare (salience 10))
   =>
   (make-instance Usuario of Lector)
   (printout t "-*-*-*-*-*-*-* Bienvenido al recomendador de libros! -*-*-*-*-*-*-*" crlf)
   (focus RECOGER_DATOS)
)

;; --------------------- MODULO RECOGER_DATOS ---------------------

(deffacts RECOGER_DATOS::hechos_iniciales "Establece hechos para poder recopilar informacion"
	(ask subgeneros-favoritos)
    (ask autor-favorito)
)
;; -------------------- FUNCIONES
(deffunction RECOGER_DATOS::hacer_pregunta_simple (?pregunta $?valores-permitidos)
    ;; Mostrar pregunta y sus opciones
    (printout t ?pregunta crlf "Introduce uno de los siguientes valores:" crlf)
    (foreach ?valor ?valores-permitidos
        (printout t ?valor crlf)
    )

    ;; Obtener respuesta valida
    (bind ?esInputCorrecto FALSE)
    (while (not ?esInputCorrecto) do
        (bind ?respuesta (read))
        (if (not (member$ (lowcase ?respuesta) ?valores-permitidos))
            then
            (printout t "Valor incorrecto. Vuelve a introducir:" crlf)
        else
            (bind ?esInputCorrecto TRUE)
        )
    )
    (printout t crlf)
    ?respuesta
)

(deffunction RECOGER_DATOS::hacer_pregunta_simple_sin_restriciones (?pregunta)
    (printout t ?pregunta crlf)
    (bind ?respuesta (read))
    (printout t crlf)
    ?respuesta
)

;; -------------------- REGLAS
(defrule RECOGER_DATOS::recoger_subgeneros_favoritos "Recoger los subgeneros preferidos del lector"
    ?hecho <- (ask subgeneros-favoritos)
    ?lector <- (object (is-a Lector))
    =>
    (bind ?generos_posibles (create$ narrativa policiaca terror fantasia romantica historica ciencia_ficcion aventura))
    (bind ?respuesta (hacer_pregunta_simple "¿Qué géneros literarios te interesan? " ?generos_posibles))
    (send ?lector put-subgeneros_preferidos ?respuesta)
    (retract ?hecho)
)

(defrule RECOGER_DATOS::recoger_autor_favorito "Recoger el autor favorito"
    ?hecho <- (ask autor-favorito)
    ?lector <- (object (is-a Lector))
    =>
    (bind ?respuesta (hacer_pregunta_simple_sin_restriciones "¿Cuál es tu autor favorito?"))
    (send ?lector put-autores_favoritos ?respuesta)
    (retract ?hecho)
)

(defrule RECOGER_DATOS::finalizar_recogida "Finaliza la recogida de informacion"
   (not (ask subgeneros-favoritos))
   (not (ask autor-favorito))
   =>
   (printout t "Procesando los datos obtenidos..." crlf)
   (focus ABSTRAER_DATOS)
)

;; MODULO ABSTRAER_DATOS
(defrule ABSTRAER_DATOS::hola ""
   =>
    (focus PROCESAR_DATOS)
)

;; MODULO PROCESAR_DATOS
(defrule PROCESAR_DATOS::filtrar_genero "Filtrar los libros por genero"
    (declare (salience 10))
    ?lector <- (object(is-a Lector))
    =>
    (bind ?genero_escogido (str-cat(send ?lector get-subgeneros_preferidos)))
    (bind ?*libros* (find-all-instances ((?inst Novela)) (member$ ?genero_escogido ?inst:subgenero)))
    (bind ?*copia_libros* ?*libros*)
    (printout t ?*libros* crlf)
)

(defrule PROCESAR_DATOS::finalizar_procesamiento ""
    ?lector <- (object(is-a Lector))
    =>
    (focus MOSTRAR_LIBROS)
)

(defrule MOSTRAR_LIBROS::mostrar_libros ""
    ?lector <- (object(is-a Lector))
    =>
    (printout t "Estos son los libros que te recomendamos:" crlf)
    (printout t ?*libros* crlf)
)