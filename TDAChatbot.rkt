#lang racket

(require "TDAFlow.rkt")
(require "TDAOption.rkt")

(provide (all-defined-out))   

#|
Dominio: cualquier dato
Recorrido: booleano
Descripción: Función de pertenencia del TDA Chatbot, para comprobar si un es o no un chatbot
|#

(define (chatbot? cb)
  (and (= (length cb) 5) (integer? (car cb)) (string? (cadr cb)) (string? (caddr cb))
       (integer? (cadddr cb)) (list? (last cb))))

#|
Dominio: Chatbot
Recorrido: int
Descripción: Función selectora que entrega el ID de un chatbot
|#

(define chatbot-get-id car)

#|
Dominio: Chatbot
Recorrido: string
Descripción: Función selectora que entrega el mensaje de bienvenida de un chatbot
|#

(define (chatbot-get-msg cb)
  (caddr cb))


#|
Dominio: Chatbot
Recorrido: string
Descripción: Función selectora que entrega el nombre de un chatbot
|#

(define (chatbot-get-name cb)
  (cadr cb))

#|
Dominio: Chatbot
Recorrido: flow list
Descripción: Función selectora que entrega una lista de flows
|#

(define (chatbot-get-flows cb)
  (last cb))

#|
Dominio: msg (string) X Chatbot
Recorrido: option list
Tipo de recursion: Ninguna
Descripción: Función selectora que entrega una lista de opciones
|#

(define (chatbot-specific-flow msg cb)
  (define flows (chatbot-get-flows cb))
  (if (null? flows)
      null
      (filter (lambda (x) (not (equal? x null))) (map (lambda (f) (flow-specific-option msg f)) flows))))

#|
Dominio: Chatbot
Recorrido: keyword list
Tipo de recursion: de cola
Descripción: Función selectora que entrega una lista de todas las keywords del chatbot
|#

(define (chatbot-get-key-list cb)
  (define (key-cola cb key-list)
    (if (null? (chatbot-get-flows cb))
        key-list
        (key-cola (list (car cb) (cadr cb) (caddr cb) (cadddr cb) (cdr (chatbot-get-flows cb)))
                  (append key-list (map option-get-keywords (last (car (chatbot-get-flows cb))))))))
  (key-cola cb '()))


