#lang racket

(require "TDAFlow_22594262_Al-Marzuk.rkt")
(require "TDAOption_22594262_Al-Marzuk.rkt")

(provide (all-defined-out))

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
Dominio: cualquier dato
Recorrido: booleano
Descripción: Función de pertenencia del TDA Chatbot, para comprobar si un es o no un chatbot
|#

(define (chatbot? cb)
  (and (= (length cb) 5) (integer? (car cb)) (string? (cadr cb)) (string? (caddr cb))
       (integer? (cadddr cb)) (list? (last cb))))

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


