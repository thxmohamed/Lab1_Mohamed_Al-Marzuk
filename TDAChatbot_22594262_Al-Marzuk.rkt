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
