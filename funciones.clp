(deffunction MAIN::pregunta_simple (?pregunta $?valores-permitidos)
    (progn$
        (?var ?valores-permitidos)
        (lowcase ?var)
    )
    (printout t ?pregunta crlf)
    (printout t "Introduce uno: " (implode$ ?valores-permitidos) crlf)
    (bind ?respuesta (read))
    (while (not (member$ (lowcase ?respuesta) ?valores-permitidos)) do
        (printout t "Valor inválido, vuelve a intentarlo." crlf)
        (bind ?respuesta (read))
    )
    (printout t crlf)
    ?respuesta
)

(deffunction MAIN::pregunta_numerica (?pregunta ?rangini ?rangfi)
    (printout t ?pregunta crlf)
    (printout t "Introduce un valor entre: [" ?rangini ", " ?rangfi "]" crlf)
    (bind ?respuesta (read))
    (while (or (not (integerp ?respuesta)) (not(and(>= ?respuesta ?rangini)(<= ?respuesta ?rangfi)))) do
        (printout t "Valor inválido, vuelve a intentarlo." crlf)
        (bind ?respuesta (read))
    )
    (printout t crlf)
    ?respuesta
)

(deffunction MAIN::pregunta_si_o_no (?pregunta)
    (bind ?respuesta (pregunta_simple ?pregunta si no))
    (if (eq (lowcase ?respuesta) si)
        then TRUE
        else FALSE
    )
)

(deffunction MAIN::pregunta_general (?pregunta)
    (printout t ?pregunta crlf)
    (bind ?respuesta (read))
    (printout t crlf)
    ?respuesta
)

(deffunction MAIN::pregunta_multiple (?pregunta $?valores-permitidos)
    (progn$
        (?var ?valores-permitidos)
        (lowcase ?var)
    )
    (printout t ?pregunta crlf)
    (printout t "Introduce en la misma línea y separados por espacio, alguno de los siguientes:" crlf (implode$ ?valores-permitidos) crlf)

    (bind ?respuesta (readline))
    (bind ?res (explode$ ?respuesta))
    
    (bind ?existeValorInvalido FALSE)
    (progn$ (?valor ?res)
        (if (not (member$ (lowcase ?valor) ?valores-permitidos))
        then (bind ?existeValorInvalido TRUE)
        )
    )
    (while ?existeValorInvalido do
        (bind ?existeValorInvalido FALSE)
        (printout t "Existe algún valor inválido, vuelve a intentarlo." crlf)
        (bind ?respuesta (readline))
        (bind ?res (explode$ ?respuesta))
        (progn$ (?valor ?res)
            (if (not (member$ (lowcase ?valor) ?valores-permitidos))
            then (bind ?existeValorInvalido TRUE)
            )
        )
    )
    (printout t crlf)
    ?res
)

(deffunction MAIN::tienen_elemento_en_comun (?list1 ?list2)
    (bind ?found FALSE)
    (foreach ?elem1 ?list1
        (bind ?el (str-cat ?elem1))
        (if (member$ ?el ?list2) then
        (bind ?found TRUE)
        (break)
        )
    )
    ?found
)