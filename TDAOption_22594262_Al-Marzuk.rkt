#lang racket

(provide (all-defined-out))

#|
Función de pertenencia del TDA Option

Dominio: cualquier dato
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
Descripción: función selectora que entrega el id de la opcion
|#

(define (option-get-id op)
  (car op))

#|
Dominio: option
Recorrido: list
Descripción: función selectora que entrega una lista con las palabras claves de la opcion
|#

(define (option-get-keywords op)
  (last op))

#|
Dominio: option
Recorrido: string
Descripción: función selectora que entrega el nombre de la opción
|#

(define (option-get-option op)
  (cadr op))
