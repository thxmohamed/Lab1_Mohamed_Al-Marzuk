#lang racket

(require "TDAOption_22594262_Al-Marzuk.rkt")

(provide (all-defined-out))

#|
RF3: TDA Flow (Constructor)

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
Dominio: Cualquier dato
Recorrido: booleano
Descripción: Función de pertenencia del TDA flow para comprobar si un elemento es un flow o no
|#

(define (flow? f)
  (and (= (length f) 3) (integer? (car f)) (string? (cadr f)) (list? (caddr f))))

#|
Dominio: Chatbot
Recorrido: booleano
Descripción: Función de pertenencia que comprueba si en una lista de flows de un chatbot se repite algun ID
|#

(define (id-repetido? id flows)
  (not (null? (filter (lambda (flow) (= id (car flow))) flows))))


#|

RF4: TDA Flow (modificador)

Dominio: Flow, Option
Recorrido: Flow
Descripción: Función que agrega un option a un flow. No se agrega si la opción se repite dentro del chatbot en base a su id
|#

(define (flow-add-option flow option)
  (if (and (null? (last flow)) (flow? flow) (option? option)) 
      (list (car flow) (cadr flow) (cons option (caddr flow))) 
      (if (and (id-repetido? (car option) (last flow)) (flow? flow) (option? option))
          flow 
          (list (car flow) (cadr flow) (cons option (caddr flow))))))

#|
Dominio: Flow
Recorrido: list options
Descripción: Función selectora del TDA Flow que entrega todas las opciones que tiene un flow
|#

(define (flow-get-options flow)
  (last flow))


#|
Dominio: Flow
Recorrido: string
Descripción: Función selectora del TDA Flow que entrega el nombre del flow
|#

(define (flow-get-name flow)
  (cadr flow))

#|
Dominio: Flow
Recorrido: list strings
Tipo de recursión: de cola
Descripción: Función selectora del TDA Flow que entrega la lista de opciones de un flow, en base a un mensaje
|#

(define (flow-specific-option msg flow)
  (define options (flow-get-options flow))
  (define (flow-option msg op)
    (if (null? op)
        null
        (if (ormap (lambda (key) (equal? msg key)) (last (car op)))
            (append (list (flow-get-name flow)) (map option-get-option options))
            (flow-option msg (cdr op)))))
  (flow-option msg options))

