;; VARIABLES GLOBALES
(defglobal ?*libros* = (create$ ""))
(defglobal ?*copia_libros* = (create$ ""))
(defglobal ?*rango_edad* = (create$ ""))
(defglobal ?*subgeneros_estado_animico* = (create$ ""))

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
   (printout t "-*-*-*-*-*-*-* Bienvenido al recomendador de novelas! -*-*-*-*-*-*-*" crlf)
   (printout t "A continuación vamos a realizarte unas preguntas para recomendarte" crlf "las novelas que más se ajusten a tu perfil!" crlf crlf)
   (focus RECOGER_DATOS)
)

;; --------------------- MODULO RECOGER_DATOS ---------------------
(defrule RECOGER_DATOS::recoger_subgeneros_favoritos "Recoger los subgeneros preferidos del lector"
    ?lector <- (object (is-a Lector))
    =>
    (bind ?quiere_responder (pregunta_si_o_no "¿Hay algún género literario que te interese?"))
    (if ?quiere_responder
        then
        (bind ?generos_posibles (create$ narrativa policiaca terror fantasia romantica historica ciencia_ficcion aventura))
        (bind ?respuesta (pregunta_multiple "¿Que géneros literarios te interesan?" ?generos_posibles))
        (send ?lector put-subgeneros_preferidos ?respuesta)
        (assert (filtra_subgenero))
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
    (bind ?respuesta (pregunta_numerica "¿Cual es tu edad? " 0 100))
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

(defrule ABSTRAER_DATOS:abstraccion_estado_animico "ira relacionado con subgenero"
    ?lector <- (object(is-a Lector))
    =>
    (bind ?estado_animico_lector (send ?lector get-estado_animico_deseado))
    (if (eq ?estado_animico_lector "relajado") then (bind ?*subgeneros_estado_animico* "narrativa" "historica")
     else (if (eq ?estado_animico_lector "intrigado") then (bind ?*subgeneros_estado_animico* "policiaca" "aventura" "ciencia_ficcion")
           else (if (eq ?estado_animico_lector "emocionado") then (bind ?*subgeneros_estado_animico* "romantica" "fantasia" "aventura")
                 else  (bind ?*subgeneros_estado_animico* "historica" "terror" "policiaca")
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