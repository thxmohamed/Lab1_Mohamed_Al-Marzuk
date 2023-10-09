#lang racket

(require "TDAOption_22594262_Al-Marzuk.rkt")

(provide (all-defined-out))

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

