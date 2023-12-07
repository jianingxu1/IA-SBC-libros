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

;; -------------------- REGLAS
(defrule RECOGER_DATOS::recoger_subgeneros_favoritos "Recoger los subgeneros preferidos del lector"
    ?hecho <- (ask subgeneros-favoritos)
    ?lector <- (object (is-a Lector))
    =>
    (bind ?generos_posibles (create$ narrativa policiaca terror fantasia romantica historica ciencia_ficcion aventura))
    (bind ?respuesta (pregunta_multiple "¿Qué géneros literarios te interesan?" ?generos_posibles))
    (send ?lector put-subgeneros_preferidos ?respuesta)
    (retract ?hecho)
)

(defrule RECOGER_DATOS::recoger_autor_favorito "Recoger el autor favorito"
    ?hecho <- (ask autor-favorito)
    ?lector <- (object (is-a Lector))
    =>
    (bind ?respuesta (pregunta_general "¿Cuál es tu autor favorito?"))
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
    (bind ?generos_escogidos (send ?lector get-subgeneros_preferidos))
    (bind ?*libros*
        (find-all-instances 
            ((?inst Novela))
            (tienen_elemento_en_comun ?generos_escogidos ?inst:subgenero)
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
            ;;(printout t ?*libros* crlf)
     )
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
;;