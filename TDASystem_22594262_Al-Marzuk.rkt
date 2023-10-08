#lang racket

(require "TDAChatbot_22594262_Al-Marzuk.rkt")
(require "TDAOption_22594262_Al-Marzuk.rkt")

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
      (list (car system) (cadr system) (caddr system)
            (cadddr system) (car (cddddr system)) (cons chatbot (last system)))
      (if (id-repetido? (car chatbot) (last system))
          system
          (list (car system) (cadr system) (caddr system)
                (cadddr system) (car (cddddr system)) (cons chatbot (last system))))))

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
      (list (car system) (cadr system) (append (caddr system) (list user)) (cadddr system)
            (append (car (cddddr system)) (list (list user))) (last system))))

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

#|
RF11: TDA System (modificador)

Dominio: system
Recorrido: system
Tipo de algoritmo: Ninguno en específico
Descripción: Funcion que deslogea a un usuario del sistema
|#
(define (system-logout system)
  (list (car system) (cadr system) (caddr system) '() (car (cddddr system)) (last system)))

#|
Dominio: system
Recorrido: lista de keywords list
Tipo de recursión: de cola
Descripción: Funcion selectora que utiliza recursión de cola para acceder a todas las keywords del system.
La idea era utilizar esta función para el RF12 pero no supe cómo.
|#

(define (system-get-key-list system)
  (define (key-cola system key-list)
    (if (null? (system-get-chatbots system))
        key-list
        (key-cola (list (car system) (cadr system) (caddr system) (cadddr system) (car (cddddr system)) (cdr (last system)))
                  (append key-list (map option-get-keywords (last (last (last (car (last system))))))))))
  (key-cola system '()))

#|
Dominio: msg X lista de keywordd list
Recorrido: booleano
Tipo de recursión: Natural
Descripción: Funcion de pertenencia que utiliza recursión Natural para determinar si un mensaje es una keyword.
Esta función estaba planeada para el RF12.
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
que contenga un mensaje. Al igual que la anterior, era para utilizarla en el RF12.
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
RF12: TDA System (modificador)

Dominio: system X msg (string)
Recorrido: system
Tipo de algoritmo: Ninguno en específico
Descripción: Funcion que sirve para hablar con un chatbot de forma recursiva. No funciona correctamente.
|#

(define (system-talk-rec system msg)
  (define (cola2 user sys chathistory n)
    (if (null? (system-get-chat-history sys))
        (list (car sys) (cadr sys) (caddr sys) (cadddr sys) chathistory (last sys))
        (if (equal? user (caar (system-get-chat-history sys)))
            (list (car system) (cadr system) (caddr system) (cadddr system)
                  (modificador (car (cddddr system)) n (append (list user (chat-history (current-seconds) user msg)) (cdr (list-ref (car (cddddr system)) n)))) (last system))
            (cola2 user (list (car sys) (cadr sys) (caddr sys) (cadddr sys) (cdr (car (cddddr sys))) (last sys))
                   (append chathistory (caar (cddddr sys))) (+ n 1)))))
  (if (null? (system-get-logged-user system))
      system
      (cola2 (system-get-logged-user system) system '() 0)))




