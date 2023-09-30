#lang racket

(provide (all-defined-out))

#|
RF7: TDA System (Constructor)

Dominio: name (strint) X ID Chatbot Inicial (int)  X Chatbots (lista)
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
