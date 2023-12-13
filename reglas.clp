;; VARIABLES GLOBALES
(defglobal ?*libros* = (create$ ""))
(defglobal ?*copia_libros* = (create$ "")) ;; en caso de que llegue alguna regla 
(defglobal ?*rango_edad* = (create$ ""))
(defglobal ?*subgeneros_estado_animico* = (create$ ""))
(defglobal ?*habito_de_lectura* = (create$ ""))
(defglobal ?*nivel_de_lectura* = (create$ ""))
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
(defrule RECOGER_DATOS::recoger_subgeneros_favoritos "Recoger los subgeneros preferidos del lector"
    ?lector <- (object (is-a Lector))
    =>
    (bind ?quiere_responder (pregunta_si_o_no "¿Hay algun genero literario que te interese?"))
    (if ?quiere_responder
        then
        (bind ?generos_posibles (create$ narrativa policiaca terror fantasia romantica historica ciencia_ficcion aventura))
        (bind ?respuesta (pregunta_multiple "¿Que genero/s literario/s te interesan?" ?generos_posibles))
        (send ?lector put-subgeneros_preferidos ?respuesta)
        (assert (filtra_subgenero))
    )
)

(defrule RECOGER_DATOS::recoger_epocas_favoritas "Recoger las epocas favoritas del lector"
    ?lector <- (object (is-a Lector))
    =>
    (bind ?quiere_responder (pregunta_si_o_no "¿Hay alguna epoca sobre la que te gustaria leer?"))
    (if ?quiere_responder
        then
        (bind ?epocas_posibles (create$ prehistoria edad_antigua edad_media edad_moderna edad_contemporanea))
        (bind ?respuesta (pregunta_multiple "¿Que epocas te interesan?" ?epocas_posibles))
        (send ?lector put-epocas_preferidas ?respuesta)
        (assert (filtra_epoca))
    )
)

(defrule RECOGER_DATOS::recoger_idiomas "Recoger los idiomas en los que quiere leer el lector"
    ?lector <- (object (is-a Lector))
    =>
    (bind ?quiere_responder (pregunta_si_o_no "¿Hay algun idioma en especifico que quieras?"))
    (if ?quiere_responder
        then
        (bind ?idiomas_posibles (create$ universal castellano ingles frances aleman chino))
        (bind ?respuesta (pregunta_multiple "¿Que idiomas te interesan?" ?idiomas_posibles))
        (send ?lector put-idiomas_preferidos ?respuesta)
        (assert (filtra_idioma))
    )
)

(defrule RECOGER_DATOS::recoger_autor_favorito "Recoger el autor favorito"
    ?lector <- (object (is-a Lector))
    =>
    (bind ?quiere_responder (pregunta_si_o_no "¿Tienes autor favorito?"))
    (if ?quiere_responder
        then
        (bind ?respuesta (pregunta_general "¿Cual es tu autor favorito? (Introduce el nombre entre comillas)"))
        (send ?lector put-autores_favoritos ?respuesta)
        (assert (filtra_autor))
    )
)

(defrule RECOGER_DATOS::recoger_edad_usuario "Recoger la edad del usuario"
    ?lector <- (object (is-a Lector))
    =>
    (bind ?respuesta (pregunta_numerica "¿Cual es tu edad? " 5 100))
    (send ?lector put-edad ?respuesta)
)

(defrule RECOGER_DATOS::recoger_estado_animico "Recoger el estado animico del usuario"
    ?lector <- (object (is-a Lector))
    =>
    (bind ?quiere_responder (pregunta_si_o_no "¿Quieres que el libro te haga sentir relajado, intrigado, emocionado o reflexivo?"))
    (if ?quiere_responder
        then
        (bind ?estados_animicos_posibles (create$ relajado intrigado emocionado reflexivo))
        (bind ?respuesta (pregunta_simple "Indique la opcion escogida por favor: " ?estados_animicos_posibles))
        (send ?lector put-estado_animico_deseado ?respuesta)
    )
)

(defrule RECOGER_DATOS::recoger_horas_lectura_semanales "Recoger las horas de lectura semanales"
    ?lector <- (object (is-a Lector))
    =>
    (bind ?respuesta (pregunta_numerica "¿Aproximadamente cuántas horas lees a la semana? " 0 168))
    (send ?lector put-horas_lectura_semanales ?respuesta)
)

(defrule RECOGER_DATOS::finalizar_recogida "Finaliza la recogida de informacion"
   (declare (salience -10))
   =>
   (printout t "Procesando los datos obtenidos..." crlf)
   (focus ABSTRAER_DATOS)
)

;; --------------------- MODULO ABSTRAER_DATOS ---------------------
(defrule ABSTRAER_DATOS::abstraccion_edad_lector "ira relacionado con publico_dirigido"
    ?lector <- (object(is-a Lector))
    =>
    (bind ?edad_lector (send ?lector get-edad))
    (if (<= ?edad_lector 12) then (bind ?*rango_edad* "infantil")
     else (if (<= ?edad_lector 18) then (bind ?*rango_edad* "adolescente")
           else (if (<= ?edad_lector 50) then (bind ?*rango_edad* "adulto")
                 else  (bind ?*rango_edad* "experimentado")
                )
           )
    )
)

(defrule ABSTRAER_DATOS::finalizar_abstraccion ""
    (declare (salience -10))
    ?lector <- (object(is-a Lector))
    =>
    (focus PROCESAR_DATOS)
)

;; --------------------- MODULO PROCESAR_DATOS ---------------------
(defrule PROCESAR_DATOS::inicializar ""
    (declare (salience 10))
    ?lector <- (object(is-a Lector))
    =>
    (bind ?*libros* (find-all-instances ((?inst Novela)) (eq ?inst:titulo ?inst:titulo)))
    (bind ?*copia_libros* ?*libros*)
)

(defrule PROCESAR_DATOS::filtrar_genero "Filtrar los libros por genero"
    ?hecho <- (filtra_subgenero)
    ?lector <- (object(is-a Lector))
    =>
    (bind ?i 1)
    (bind ?aux (create$))
    
    (bind ?generos_escogidos (send ?lector get-subgeneros_preferidos))
    
    (while (<= ?i (length$ ?*libros*)) do
        (bind ?libro_nth (nth$ ?i ?*libros*))
        (bind ?var_subgeneros (send ?libro_nth get-subgenero))
        (if (tienen_elemento_en_comun ?generos_escogidos ?var_subgeneros)
            then (bind ?aux (create$ ?aux ?libro_nth)))
        (bind ?i (+ ?i 1))
    )   
    (bind ?*libros* ?aux)

    ;; Si libros se queda en 0, no modificar copia_libros
    (if (not (= (length$ ?*libros*) 0)) 
        then (bind ?*copia_libros* ?*libros*) 
    )
    (retract ?hecho)
)

(defrule PROCESAR_DATOS::filtrar_autor "Filtrar los libros por autor"
    ?hecho <- (filtra_autor)
    ?lector <- (object(is-a Lector))
    =>
     (bind ?autor_escogido (str-cat(send ?lector get-autores_favoritos)))
     
     ;;Buscamos la instancia de Escritor cuyo nombre es ?autor_escogido
     (bind ?instancia_autor_escogido (find-instance ((?inst Escritor)) (eq ?autor_escogido ?inst:nombre)))

     (if (= (length$ ?instancia_autor_escogido) 0)
          then (printout t "No existe ese autor en la base de datos" crlf)
          else
            (bind ?i 1)
            (bind ?aux (create$))
            (while (<= ?i (length$ ?*libros*)) do
               (bind ?libro_nth (nth$ ?i ?*libros*))
               (bind ?autor_libro (send ?libro_nth get-escrito_por)) 
               (if (eq ?autor_escogido (send ?autor_libro get-nombre))
                   then (bind ?aux (create$ ?aux ?libro_nth)))
               (bind ?i (+ ?i 1))
            )   
            (bind ?*libros* ?aux)
     )

    ;; Si libros se queda en 0, no modificar copia_libros
    (if (not (= (length$ ?*libros*) 0)) 
        then (bind ?*copia_libros* ?*libros*) 
    )
    (retract ?hecho)
)

(defrule PROCESAR_DATOS::filtrar_publico_dirigido "Filtrar los libros por publico_dirigido"
    ?lector <- (object(is-a Lector))
    =>
    (bind ?i 1)
    (bind ?aux (create$))
    
    (while (<= ?i (length$ ?*libros*)) do
        (bind ?libro_nth (nth$ ?i ?*libros*))
        (bind ?var_publico_dirigido (send ?libro_nth get-publico_dirigido)) 
        (if (or (member$ ?*rango_edad* ?var_publico_dirigido) (member$ "para_todos" ?var_publico_dirigido))
            then (bind ?aux (create$ ?aux ?libro_nth)))
        (bind ?i (+ ?i 1))
    )   
    (bind ?*libros* ?aux)

    ;; Si libros se queda en 0, no modificar copia_libros
    (if (not (= (length$ ?*libros*) 0)) 
        then (bind ?*copia_libros* ?*libros*) 
    )
)

(defrule PROCESAR_DATOS::filtrar_estado_animico "Filtrar los libros por estado animico deseado"
    ?lector <- (object(is-a Lector))
    =>
    (bind ?i 1)
    (bind ?aux (create$))
    
    (while (<= ?i (length$ ?*libros*)) do
        (bind ?libro_nth (nth$ ?i ?*libros*))
        (bind ?var_subgeneros (send ?libro_nth get-subgenero))
        (if (tienen_elemento_en_comun ?*subgeneros_estado_animico* ?var_subgeneros)
            then (bind ?aux (create$ ?aux ?libro_nth)))
        (bind ?i (+ ?i 1))
    )   
    (bind ?*libros* ?aux)

    ;; Si libros se queda en 0, no modificar copia_libros
    (if (not (= (length$ ?*libros*) 0)) 
        then (bind ?*copia_libros* ?*libros*) 
    )

)

(defrule PROCESAR_DATOS::filtrar_extension "Filtrar los libros por extension relacionado con nivel de lectura"
    ?lector <- (object(is-a Lector))
    =>
    (bind ?i 1)
    (bind ?aux (create$))
    (switch ?*nivel_de_lectura*
        (case "principiante" then
            (bind ?extension_deseada "corta")
            (printout t "switch" crlf)
        )
        (case "intermedio" then
            (bind ?extension_deseada "media")
        )
        (case "avanzado" then (bind ?extension_deseada "larga"))
        (default (bind ?extension_deseada "larga"))
    )

    (while (<= ?i (length$ ?*libros*)) do
        (bind ?libro_nth (nth$ ?i ?*libros*))
        (bind ?var_extension (send ?libro_nth get-extension)) 
        (if (eq ?extension_deseada ?var_extension)
            then (bind ?aux (create$ ?aux ?libro_nth))
        )
        (bind ?i (+ ?i 1))
    )   
    (bind ?*libros* ?aux)

    ;; Si libros se queda en 0, no modificar copia_libros
    (if (not (= (length$ ?*libros*) 0)) 
        then (bind ?*copia_libros* ?*libros*) 
    )
)

(defrule PROCESAR_DATOS::filtrar_epoca "Filtrar los libros por epoca"
    ?hecho <- (filtra_epoca)
    ?lector <- (object(is-a Lector))
    =>
    (bind ?i 1)
    (bind ?aux (create$))
    (bind ?epocas_escogidas (send ?lector get-epocas_preferidas))
    (while (<= ?i (length$ ?*libros*)) do
        (bind ?libro_nth (nth$ ?i ?*libros*))   
        (bind ?var_epoca (send ?libro_nth get-epoca))

        (bind ?j 1)
        (while (<= ?j (length$ ?epocas_escogidas)) do
            (bind ?epoca_nth (nth$ ?j ?epocas_escogidas))
            (if (eq (str-cat ?epoca_nth) (str-cat ?var_epoca))
                then (bind ?aux (create$ ?aux ?libro_nth))
            )
            (bind ?j (+ ?j 1))
        )
        (bind ?i (+ ?i 1))        
    )   
    (bind ?*libros* ?aux)

    ;; Si libros se queda en 0, no modificar copia_libros
    (if (not (= (length$ ?*libros*) 0)) 
        then (bind ?*copia_libros* ?*libros*) 
    )
    (retract ?hecho)
)

(defrule PROCESAR_DATOS::filtrar_idioma "Filtrar los libros por idioma"
    ?hecho <- (filtra_idioma)
    ?lector <- (object(is-a Lector))
    =>
    (bind ?i 1)
    (bind ?aux (create$))
    
    (bind ?idiomas_escogidos (send ?lector get-idiomas_preferidos))
    
    (while (<= ?i (length$ ?*libros*)) do
        (bind ?libro_nth (nth$ ?i ?*libros*))
        (bind ?var_idiomas (send ?libro_nth get-idiomas))
        (if (tienen_elemento_en_comun ?idiomas_escogidos ?var_idiomas)
            then (bind ?aux (create$ ?aux ?libro_nth)))
        (bind ?i (+ ?i 1))
    )   
    (bind ?*libros* ?aux)

    ;; Si libros se queda en 0, no modificar copia_libros
    (if (not (= (length$ ?*libros*) 0)) 
        then (bind ?*copia_libros* ?*libros*) 
    )
    (retract ?hecho)
)


(defrule PROCESAR_DATOS::finalizar_procesamiento "Funcion que finaliza el procesado"
    (declare (salience -10))
    ?lector <- (object(is-a Lector))
    =>
    (if (= (length$ ?*libros*) 0) 
        then (bind ?*libros* ?*copia_libros*) 
    )
    (focus MOSTRAR_LIBROS)
)

(defrule MOSTRAR_LIBROS::mostrar_libros "Funcion que muestra los libros recomendados"
    ?lector <- (object(is-a Lector))
    =>
    
    (bind ?i 1)
    (bind ?aux (create$))

    (while (and (<= ?i (length$ ?*libros*)) (< (length$ ?aux) 3)) do
        (bind ?libro_nth (nth$ ?i ?*libros*))
        (bind ?var_valoracion (send ?libro_nth get-valoracion))
        (if (eq ?var_valoracion "excelente")
            then (bind ?aux (create$ ?aux ?libro_nth))
        )

        (bind ?i (+ ?i 1))
    )
        
    (bind ?j 1)
    (while (and (<= ?j (length$ ?*libros*)) (< (length$ ?aux) 3)) do
        (bind ?libro_nth (nth$ ?j ?*libros*))
        (bind ?var_valoracion (send ?libro_nth get-valoracion))
        (if (eq ?var_valoracion "buena")
            then (bind ?aux (create$ ?aux ?libro_nth))
        )

        (bind ?j (+ ?j 1))
    )

    (bind ?k 1)
    (while (and (<= ?k (length$ ?*libros*)) (< (length$ ?aux) 3)) do
        (bind ?libro_nth (nth$ ?k ?*libros*))
        (bind ?var_valoracion (send ?libro_nth get-valoracion))
        (if (eq ?var_valoracion "regular")
            then (bind ?aux (create$ ?aux ?libro_nth))
        )

        (bind ?k (+ ?k 1))
    )

    (bind ?z 1)
    (while (and (<= ?z (length$ ?*libros*)) (< (length$ ?aux) 3)) do
        (bind ?libro_nth (nth$ ?z ?*libros*))
        (bind ?var_valoracion (send ?libro_nth get-valoracion))
        (if (eq ?var_valoracion "deficiente")
            then (bind ?aux (create$ ?aux ?libro_nth))
        )

        (bind ?z (+ ?z 1))
    )

    (bind ?*libros* ?aux)  
    (printout t "Estos son los libros que te recomendamos:" crlf)
    (printout t ?*libros* crlf)
)