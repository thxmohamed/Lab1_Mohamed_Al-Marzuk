#lang racket

(require "TDAOption.rkt")
(require "TDAFlow.rkt")
(require "TDAChatbot.rkt")
(require "TDASystem.rkt")
(require "TDAChatHistory.rkt")

(provide (all-defined-out))

#|
RF2: TDA Option (Constructor)

Dominio: code (Int) X message (String)  X ChatbotCodeLink (Int) X InitialFlowCodeLink (Int) X Keywords* (lista de palabras claves)
Recorrido: option (lista con todos los elementos del dominio)
Tipo de algoritmo: Ninguno en específico
Descripción: Función que construye una lista, compuesta por: numero de la opcion, opción, codigo del chatbot
flujo del chatbot, y por ultimo una lista con las palabras claves asociadas al chatbot

|#

(define (option code text chatbot-code-link flow-code-link keyword . choices)
  (list code text chatbot-code-link flow-code-link (append choices (list keyword))))

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
RF5: TDA Chatbot (Constructor)

Dominio: Chatbot id (int) X Chatbot-name (String)  X WelcomeMsg (string) X StartFlowID (int) X Flows*  (Indica que puede recibir cero o más flows)
Recorrido: Chatbot (lista con todos los elementos del dominio)
Tipo de algoritmo: Ninguno en específico
Descripción: Función que construye una lista, compuesta por: id del chatbpot, nombre del chatbot
un mensaje de bienvenida, un id del flow inicial y una lista de flows asociados al chatbot
|#

(define (chatbot chatbotID name welcomeMessage startFlowID . flows)
  (define flows-sin-repetidos (remove-duplicates flows))
  (list chatbotID name welcomeMessage startFlowID flows-sin-repetidos))


#|
RF6: TDA Chatbot (Modificador)
Dominio: Chatbot, Flow
Recorrido: Chatbot
Tipo de recursion: de cola
Descripción: Función que agrega recursivamente un flow a un chatbot haciendo recursion de cola.
Se comprueba si es nula la lista de flows, si es así, se devuelve un chatbot que contenga como lista
de flows la variable auxiliar "list-flow", y si no es nula, se comprueba si el id del flow no se repite en base
a su id, si no se repite, se agrega el car de flow a list-flow y se hace el llamado recursivo  con el mismo chatbot,
pero con el cdr de la lista de flows. Si se repite el id, se devuelve el chatbot original sin ningun cambio.
LA función exterior se llama con el chatbot y una lista que tenga como unico elemento el flow
|#

(define (chatbot-add-flow chatbot flow)  
  (define (add-flow2 chatbot list-flow)    
    (if (null? (last chatbot))        
        (list (car chatbot) (cadr chatbot) (caddr chatbot) (cadddr chatbot) list-flow)        
        (if (not (id-repetido? (car flow) (last chatbot)))
                          (add-flow2 (list (car chatbot) (cadr chatbot) (caddr chatbot)
                                          (cadddr chatbot) (cdr (last chatbot)))
                   (append (list (car (last chatbot))) list-flow))                          
                          chatbot)))
  (add-flow2 chatbot (list flow)))

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
            (append (car (cddddr system)) (list (chat-history user))) (last system))))

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
RF12: TDA System (modificador)

Dominio: system X msg (string)
Recorrido: system
Tipo de recursion: Recursion de cola
Descripción: Funcion que sirve para hablar con un chatbot de forma recursiva. Recibe un system y un mensaje.
Lo que hace es ver si hay un usuario logeado, si no lo hay, devuelve el mismo system sin ningún cambio, y si
mensaje del usuario no es ninguna keyword que el sistema pueda reconocer, también devuelve el system sin
cambios, y si el mensaje es una keyword, lo que hace es llamar a la función recursiva. Esta función lo que hace
es recorrer el chat history hasta que este esté vacío, y va revisandolo hasta encontrar la lista que contiene
el usuario logeado, cuando lo encuentra, agrega el mensaje del chatbot y el mensaje del usuario.
|#

(define (system-talk-rec system msg)
  (define (cola2 user sys chathistory n)
    (if (null? (system-get-chat-history sys))
        (list (car sys) (cadr sys) (caddr sys) (cadddr sys) chathistory (last sys))
        (if (equal? user (caar (system-get-chat-history sys)))
            (list (car system) (cadr system) (caddr system) (cadddr system)
                  (modificador (car (cddddr system)) n (append (list user (chat-msg (current-seconds) user msg)) (system-specific-chatbot msg system) (cdr (list-ref (car (cddddr system)) n)))) (last system))
            (cola2 user (list (car sys) (cadr sys) (caddr sys) (cadddr sys) (cdr (car (cddddr sys))) (last sys))
                   (append chathistory (caar (cddddr sys))) (+ n 1)))))
  (if (null? (system-get-logged-user system))
      system
      (if (not (msg-is-keyword? msg (system-get-key-list system)))
          system
          (cola2 (system-get-logged-user system) system '() 0))))

#|
RF13: TDA System (modificador)

Dominio: system X msg (string)
Recorrido: system
Tipo de algoritmo: Ninguno en específico
Descripción: Funcion que sirve para hablar con un chatbot de forma no recursiva. No funciona correctamente.
Solamente agrega los mensajes del usuario al chat history.
|#

(define (system-talk-norec system msg)
  (define user (system-get-logged-user system))
  (if (null? user)
      system      
      (list (car system) (cadr system) (caddr system) (cadddr system)
            (add-chat-history user msg (car (cddddr system))) (last system))))

#|
RF14: TDA System (modificador)

Dominio: system X user (string)
Recorrido: string
Tipo de algoritmo: Ninguno en específico
Descripción: Funcion que sirve para acceder al chat-history de un usuario en específico.
Primero se comprueba si el usuario está registrado, si no es así, devuelve un mensaje de error,
y si está registrado, devuelve la lista de su chat history, utilizando la función filter en
base al nombre del usuario.
|#

(define (system-synthesis system user)
  (if (not (member user (system-get-register-user system)))
      (display "No hay ningún chat history para ese usuario")
      (write (reverse (cdr (car (filter (lambda (sub2) (equal? user (car sub2))) (system-get-chat-history system))))))))

