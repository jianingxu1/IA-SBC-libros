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
    (bind ?respuesta (pregunta_simple ?pregunta si no s n))
    (if (or (eq (lowcase ?respuesta) si) (eq (lowcase ?respuesta) s))
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
