#lang racket

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
Dominio: Chatbot
Recorrido: booleano
Descripción: Función de pertenencia que comprueba si en una lista de flows de un chatbot se repite algun ID
|#

(define (id-repetido? id flows)
  (not (null? (filter (lambda (flow) (= id (car flow))) flows))))

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
Descripción: Función selectora que entrega elID de un chatbot
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


