;; VARIABLES GLOBALES
(defglobal ?*libros* = (create$ ""))
(defglobal ?*copia_libros* = (create$ "")) ;; en caso de que llegue alguna regla 
(defglobal ?*rango_edad* = (create$ ""))
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
    (ask edad-lector)
)

;; -------------------- REGLAS
(defrule RECOGER_DATOS::recoger_subgeneros_favoritos "Recoger los subgeneros preferidos del lector"
    ?hecho <- (ask subgeneros-favoritos)
    ?lector <- (object (is-a Lector))
    =>
    (bind ?generos_posibles (create$ narrativa policiaca terror fantasia romantica historica ciencia_ficcion aventura NO))
    (bind ?respuesta (pregunta_multiple "¿Que generos literarios te interesan?" ?generos_posibles))
    (send ?lector put-subgeneros_preferidos ?respuesta)
    (retract ?hecho)
)

(defrule RECOGER_DATOS::recoger_autor_favorito "Recoger el autor favorito"
    ?hecho <- (ask autor-favorito)
    ?lector <- (object (is-a Lector))
    =>
    (bind ?respuesta (pregunta_general "¿Cual es tu autor favorito? (Introduce el nombre entre comillas o \"NO\" en el caso de que no tengas un autor favorito)"))
    (send ?lector put-autores_favoritos ?respuesta)
    (retract ?hecho)
)

(defrule RECOGER_DATOS::recoger_edad_usuario "Recoger la edad del usuario"
    ?hecho <- (ask edad-lector)
    ?lector <- (object (is-a Lector))
    =>
    (bind ?respuesta (pregunta_numerica "¿Cual es tu edad? " 0 100))
    (send ?lector put-edad ?respuesta)
    (retract ?hecho)
)

(defrule RECOGER_DATOS::recoger_estado_animico "Recoger el estado animico"
    ?hecho <- (ask estado-animico)
    ?lector <- (object (is-a Lector))
    =>
    (bind ?estados_animicos_posibles (create$ relajado intrigado emocionado reflexivo NO))
    (bind ?respuesta (hacer-pregunta-simple "Quieres que el libro te haga sentir: " ?estados_animicos_posibles "(Introduce \"NO\" en el caso de que no te importe)"))
    (printout t ?respuesta crlf)
    (send ?lector put-estado_animico_deseado ?respuesta)
)

(defrule RECOGER_DATOS::finalizar_recogida "Finaliza la recogida de informacion"
   (not (ask subgeneros-favoritos))
   (not (ask autor-favorito))
   (not (ask edad-lector))
   =>
   (printout t "Procesando los datos obtenidos..." crlf)
   (focus ABSTRAER_DATOS)
)

;; --------------------- MODULO ABSTRAER_DATOS ---------------------
(defrule ABSTRAER_DATOS::abstraccion_estado_animico "ira relacionado con el subgenero"
    ?lector <- (object(is-a Lector))
   =>
)

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
    (focus PROCESAR_DATOS)
)

(defrule ABSTRAER_DATOS::abstraccion_lugar_lectura "ira relacionado con el formato"

   =>
    (focus PROCESAR_DATOS)
)

(defrule ABSTRAER_DATOS::abstraccion_habito_lectura "ira relacionado con la extension y el formato maybe"

   =>
    (focus PROCESAR_DATOS)
)

;; --------------------- MODULO PROCESAR_DATOS ---------------------
(defrule PROCESAR_DATOS::filtrar_genero "Filtrar los libros por genero"
    (declare (salience 10))
    ?lector <- (object(is-a Lector))
    =>
    (bind ?generos_escogidos (send ?lector get-subgeneros_preferidos))
    (if (eq ?generos_escogidos "NO") 
        then (bind ?*libros* (find-all-instances ((?inst Novela))))
        else (bind ?*libros*
             (find-all-instances 
                ((?inst Novela))
                (tienen_elemento_en_comun ?generos_escogidos ?inst:subgenero)
             )
        )
    )
    (bind ?*copia_libros* ?*libros*)
)

(defrule PROCESAR_DATOS::filtrar_autor "Filtrar los libros por autor"
    (declare (salience 9))
    ?lector <- (object(is-a Lector))
    =>
     (bind ?autor_escogido (str-cat(send ?lector get-autores_favoritos)))
     
     ;;Buscamos la instancia de Escritor cuyo nombre es ?autor_escogido
     (bind ?instancia_autor_escogido (find-instance ((?inst Escritor)) (eq ?autor_escogido ?inst:nombre)))

     (if (= (length$ ?instancia_autor_escogido) 0)
          then (printout t "No existe ese autor en la base de datos" crlf)
          else (printout t ?autor_escogido crlf)

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
)

(defrule PROCESAR_DATOS::filtrar_publico_dirigido "Filtrar los libros por publico_dirigido"
    (declare (salience 8))
    ?lector <- (object(is-a Lector))
    =>
    (bind ?i 1)
    (bind ?aux (create$))
    (while (<= ?i (length$ ?*libros*)) do
        (bind ?libro_nth (nth$ ?i ?*libros*))
        (bind ?autor_libro (send ?libro_nth get-publico_dirigido)) 
        (if (eq ?*rango_edad (send ?autor_libro get-nombre))
            then (bind ?aux (create$ ?aux ?libro_nth)))
        (bind ?i (+ ?i 1))
    )   
    (bind ?*libros* ?aux)

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