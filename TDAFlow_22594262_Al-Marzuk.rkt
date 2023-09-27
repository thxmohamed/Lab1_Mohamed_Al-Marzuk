#lang racket

#|
RF2: TDA Flow (Constructor)

Dominio: id (int) X name-msg (String)  X Option*  (Indica que puede recibir cero o más opciones)
Recorrido: flow (lista con todos los elementos del dominio)
Tipo de algoritmo: Ninguno en específico
Descripción: Función que construye una lista, compuesta por: id del flujo, nombre del flujo
y una lista de las opciones asociadas al flujo
|#

(define (flow id name . options)
  (define options-sin-duplicados
  (remove-duplicates options))
  (list id name options-sin-duplicados))

#|
Función de pertenencia del TDA flow para comprobar si un elemento es un flow o no
|#

(define (flow? f)
  (and (= (length f) 3) (integer? (car f)) (string? (cadr f)) (list? (caddr f))))
