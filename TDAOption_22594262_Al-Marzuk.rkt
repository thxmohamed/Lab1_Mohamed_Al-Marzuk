#lang racket


#|
RF2: TDA Option (Constructor)

Dominio: code (Int) X message (String)  X ChatbotCodeLink (Int) X InitialFlowCodeLink (Int) X Keywords* (lista de palabras claves)
Recorrido: option (lista con todos los elementos del dominio
Tipo de algoritmo: Ninguno en específico
Descripción: Función que construye una lista, compuesta por: numero de la opcion, opción, codigo del chatbot
flujo del chatbot, y por ultimo una lista con las palabras claves asociadas al chatbot

|#

(define (option code text chatbot-code-link flow-code-link keyword . choices)
  (list code text chatbot-code-link flow-code-link (append choices (list keyword))))

#|
Función de pertenencia del TDA Option

Dominio: option
Recorrido: booleano
Descripción: se comprueba si un dato es una opción, viendo si cumple con la misma estructura
de la opcion
|#

(define (option? op)
  (and (= (length op) 5) (integer? (car op)) (string? (cadr op)) (integer? (caddr op))
       (integer? (cadddr op)) (list? (last op))))

#|
Dominio: option
Recorrido: int
Descripción: función que entrega el id de la opcion
|#

(define (option-get-id op)
  (car op))

#|
Dominio: option
Recorrido: list
Descripción: función que entrega una lista con las palabras claves de la opcion
|#

(define (get-keywords op)
  (last op))
