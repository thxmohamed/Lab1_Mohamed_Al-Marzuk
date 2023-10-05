#lang racket

(require "TDAChatbot_22594262_Al-Marzuk.rkt")

(provide (all-defined-out))

#|
RF7: TDA System (Constructor)

Dominio: name (string) X ID Chatbot Inicial (int)  X Chatbots (lista)
Recorrido: system (lista con todos los elementos del dominio)
Tipo de algoritmo: Ninguno en específico
Descripción: Función que construye una lista, compuesta por: nombre del sistema
el id del chatbot inicial y una lista de chatbots. Además, se agregan tres
listas vacías, la primera es para los usuarios registrados, la segunda es para
los usuarios logeados (solo puede haber uno a la vez), la tercera es para almacenar el chat history.
Se asume que al momento de crear un sistema, no hay usuarios registrados ni logeados.
|#

(define (system name InitialChatbotCodeLink . chatbots)
  (define chatbots-sin-duplicados
    (remove-duplicates chatbots))
  (list name InitialChatbotCodeLink '() '() '() chatbots-sin-duplicados))

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
Función constructora de chat-history

Dominio: time (int) x sender (string) x msg (string)
Recorrido: chat-history 
Descripción: Función que construye una lista con los elementos del dominio.
|#

(define (chat-history time sender msg)
  (list time sender msg))

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

(define system-get-logged-user cadddr)

#|
Dominio: system
Recorrido: lista de chatbots
Descripción: función selectora que entrega una lista con todos los chatbots del sistema
|#

(define (system-get-chatbots system)
  (last system))

#|
RF8: TDA System (modificador)

Dominio: system X chatbot
Recorrido: system
Tipo de algoritmo: Ninguno en específico
Descripción: Funcion que agrega un chatbot a un sistema, siempre y cuando su id no se repita.
Se utiliza la función id-repetido? para comprobar si el id ya está dentro del sistema, si lo está,
devuelve el mismo sistema sin cambios, y si no lo está, agrega el chatbot al sistema.
|#

(define (system-add-chatbot system chatbot)
  (if (null? (last system))
      (list (car system) (cadr system) (caddr system) (cadddr system) (car (cddddr system)) (cons chatbot (last system)))
      (if (id-repetido? (car chatbot) (last system))
          system
          (list (car system) (cadr system) (caddr system) (cadddr system) (car (cddddr system)) (cons chatbot (last system))))))

#|
RF9: TDA System (modificador)

Dominio: system X user (string)
Recorrido: system
Tipo de algoritmo: Ninguno en específico
Descripción: Funcion que agrega un user a un sistema, siempre y cuando no esté ya dentro del sistema.
Se utiliza la función member para comprobar si el usuario ya está registrado, si es así, devuelve
el mismo sistema, y si no, devuelve el sistema, pero con el usuario nuevo registrado.
|#

(define (system-add-user system user)
  (if (member user (caddr system))
      system
      (list (car system) (cadr system) (append (caddr system) (list user)) (cadddr system) (car (cddddr system)) (last system))))

#|
RF10: TDA System (modificador)

Dominio: system X user (string)
Recorrido: system
Tipo de algoritmo: Ninguno en específico
Descripción: Funcion que logea un user a un sistema, siempre y cuando no haya nadie logeado, y
este esté registrado en el sistema
|#

(define (system-login system user)
  (if (and (member user (caddr system)) (null? (cadddr system)))
      (list (car system) (cadr system) (caddr system) (append (cadddr system) (list user))
            (car (cddddr system)) (last system))
      system))



