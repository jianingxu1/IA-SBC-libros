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