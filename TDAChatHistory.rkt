#lang racket

(provide (all-defined-out))

#|
Función constructora de un mensaje

Dominio: time (int) x sender (string) x msg (string)
Recorrido: msg 
Descripción: Función que construye una lista con los elementos del dominio.
|#

(define (chat-msg time sender msg)
  (list time sender msg))

#|
Función constructora de un chat-history

Dominio: user X chat-msg
Recorrido: chat-history
Descripción: Función que construye una lista con los elementos del dominio.
|#

(define (chat-history user . msg)
  (append (list user) msg))

#|
Función de pertenencia de chat-history

Dominio: cualquier dato
Recorrido: booleano 
Descripción: Función que entrega un booleano si es o no un chat-msg
|#

(define (chat-msg? ch)
  (and (= (length ch) 3) (string? (cadr ch)) (string? (caddr ch)) (integer? (car ch))))

#|
Función modificadora de chat-history

Dominio: time (int) x sender (string) x msg (string)
Recorrido: chat-history 
Descripción: Función que construye una lista con los elementos del dominio.
|#

(define (add-chat-history user msg l)
  (append (filter (lambda (sub) (not (equal? user (car sub)))) l)
          (list (append (car (filter (lambda (sub2) (equal? user (car sub2))) l))
                  (list (chat-msg (current-seconds) user msg))))))