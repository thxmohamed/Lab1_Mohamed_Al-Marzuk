#lang racket

(require "TDAChatbot_22594262_Al-Marzuk.rkt")

(provide (all-defined-out))

#|
Función de pertenencia del TDA Flow

Dominio: cualquier dato
Recorrido: booleano
Descripción: se comprueba si un dato es un system, viendo si cumple con la misma estructura
del system
|#

(define (system? s)
  (and (= (length s) 5) (string? (car s)) (integer? (cadr s)) (list? (caddr s)) (list? (cadddr s)) (list? (last s))))

#|
Dominio: system
Recorrido: lista de chat-historys
Descripción: función selectora que entrega una lista con el chat-history del usuario
|#

(define (system-get-chat-history system)
  (car (cddddr system)))

#|
Dominio: system
Recorrido: lista de users (solo uno, o lista vacía si no hay nadie loggeado)
Descripción: función selectora que entrega una lista con el unico usuario loggeado
|#

(define (system-get-logged-user system)
  (if (null? (cadddr system))
      '()
      (car (cadddr system))))

#|
Dominio: system
Recorrido: lista de chatbots
Descripción: función selectora que entrega una lista con todos los chatbots del sistema
|#

(define (system-get-chatbots system)
  (last system))

#|
Dominio: system
Recorrido: lista de chathistory
Descripción: función selectora que entrega una lista con todos los chathistory de cada usuario del sistema
|#

(define system-get-register-user caddr)

#|
Dominio: lista, índice (int), valor
Recorrido: lista
Descripción: función auxiliar que modifica un elemento de la lista según su índice
|#

(define (modificador lista i value)
  (if (= i 0)
      (cons value (cdr lista)) ; Modificar el primer elemento
      (cons (car lista) (modificador (cdr lista) (- i 1) value))))

#|
Dominio: system
Recorrido: lista de keywords list
Tipo de recursión: de cola
Descripción: Funcion selectora que utiliza recursión de cola para acceder a todas las keywords del system.
|#

(define (system-get-key-list system)
  (define (key-cola system key-list)
    (if (null? (system-get-chatbots system))
        key-list
        (key-cola (list (car system) (cadr system) (caddr system) (cadddr system) (car (cddddr system)) (cdr (last system)))
                  (append key-list (chatbot-get-key-list (car (last system)))))))
  (key-cola system '()))

#|
Dominio: msg X lista de keywordd list
Recorrido: booleano
Tipo de recursión: Natural
Descripción: Funcion de pertenencia que utiliza recursión Natural para determinar si un mensaje es una keyword.
|#

(define (msg-is-keyword? msg key-list)
  (if (null? key-list)
      #f
      (if (member msg (car key-list))
          #t
          (msg-is-keyword? msg (cdr key-list)))
      ))

#|
Dominio: msg (string) X system
Recorrido: keywords list
Tipo de recursión: de cola
Descripción: Funcion selectora que utiliza recursión de cola para acceder a la lista de keywords
que contenga un mensaje. Esta función era para ser utilizada en el RF12 pero no supe cómo.
|#

(define (system-get-key-list2 msg system)
  (define (cola2 msg aux-list)
    (if (and (not (null? (cdr aux-list))) (member msg (car aux-list)))
        (car aux-list)
        (cola2 msg (cdr aux-list))))
  (if (msg-is-keyword? msg (system-get-key-list system))
      (cola2 msg (system-get-key-list system))
      '()))

#|
Dominio: msg (string) X system
Recorrido: option list
Descripción: Funcion selectora que sirve para acceder a las opciones según la keyword.
|#

(define (system-specific-chatbot msg system)
  (define chatbots (system-get-chatbots system))
  (if (null? chatbots)
      null
      (car (filter (lambda (x) (not (equal? x null))) (map (lambda (cb) (chatbot-specific-flow msg cb)) chatbots)))))





