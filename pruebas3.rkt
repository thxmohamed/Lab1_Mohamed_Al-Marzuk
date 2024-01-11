#lang racket

(require "main.rkt")

(provide (all-defined-out))

#|Se crean 3 opciones para añadirlas al flujo principal del chatbot principal|#

(define op1 (option  1 "1) Jugar" 2 1 "jugar" "ocio" "divertirme"))
(define op2 (option  2 "2) Entrenar" 3 1 "ejercicio" "entrenar" "perfeccionarme" "fortalecerme"))
(define op3 (option  3 "3) Musica" 4 1 "fiesta" "bailar" "musica" "cantar" "escuchar"))

#|se crea el flujo principal|#

(define f10 (flow 1 "Flujo Principal Chatbot 1\nBienvenido\n¿Qué te gustaría hacer?" op1 op2 op2 op3 op2 op1))

(define f11 (flow-add-option f10 op3)) ;se intenta agregar la opcion 3 pero no se agrega

;chatbot inicial

(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f11 f11 f11 f11))

;definición de opciones para los siguientes chatbots

(define op4 (option 1 "1) Minecraft" 1 1 "mc" "minecraft" "cubos" "maincra"))
(define op5 (option 2 "2) Valorant" 1 1 "valorant" "valo" "disparos"))
(define op6 (option 3 "3) Fortnite" 1 2 "fortnite" "battle royale"))
(define op7 (option 4 "4) Grand Theft Auto" 1 1 "GTA" "gta" "gta v" "gta 5" "gta cinco" "grand theft auto"))
(define op8 (option 5 "5) Quiero hacer otra cosa" 0 1 "volver" "regresar"))

(define op17 (option 1 "1) Solo" 1 2 "yo" "solo" "singleplayer"))
(define op18 (option 2 "2) Duo" 1 2 "duo" "parejas"))
(define op19 (option 3 "3) Squad" 1 2 "equipo" "4 personas" "amigos" "cuatro"))
(define op20 (option 4 "4) Quiero jugar otra cosa" 1 1 "jugar otro"))

(define op9 (option 1 "1) Cardio" 1 3 "cardio" "corazon" "correr"))
(define op10 (option 2 "2) Pesas" 1 1 "mancuernas" "pesas" "musculos")) 
(define op11 (option 3 "3) Cuerda" 1 3 "saltar" "cuerda" "brincar")) 
(define op12 (option 4 "4) Quiero hacer otra cosa" 1 3 "atras" "nada"))

(define op13 (option 1 "1) Trap" 1 3 "trap"))
(define op14 (option 2 "2) K-pop" 1 2 "k-pop" "kpop" "coreana"))
(define op15 (option 3 "3) Cumbia" 1 1 "cumbia"))
(define op16 (option 4 "4) Quiero hacer otra cosa" 1 1 "otro" "nada"))

;Se crea un flujo con el primer grupo de opciones

(define f20 (flow 1 "Flujo 1 Chatbot1\n¿¿Qué te gustaría jugar?" op4 op5 op6 op7))

;se agrega otra opción al flujo

(define f201 (flow-add-option f20 op8))

(define f21 (flow 2 "Flujo 2 Chatbot1\n¿Qué modalidad quieres jugar?" op17 op18 op19 op20))
(define f30 (flow 1 "Flujo 1 Chatbot1\n¿Qué te gustaría entrenar?" op9 op10 op11 op12))
(define f40 (flow 1 "Flujo 1 Chatbot1\n¿Qué quieres escuchar?" op13 op14 op15 op16))

;se crean los chatbots con sus respectivos flujos

(define cb1 (chatbot 1 "Chat de ocio"  "Bienvenido\n¿Qué vas a jugar?" 1 f201 f21))
(define cb2 (chatbot 2 "Chat de música"  "Bienvenido\n¿Qué quieres escuchar?" 1 f30))
(define cb3 (chatbot 3 "Chat de entrenamiento"  "Bienvenido\n¿Qué quieres entrenar?" 1 f40))

;Se crea un system con los chatbots anteriores

(define s0 (system "Chatbots de prueba" 0 cb0 cb0 cb0 cb1 cb2 cb3 cb3))

(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
(define s2 (system-add-user s1 "user1")) ;se añade "user1" al registro
(define s3 (system-add-user s2 "user2")) ;se añade "user2" al registro
(define s4 (system-add-user s3 "user2")) ;no se añade nada
(define s5 (system-add-user s4 "user3")) ;se añade "user3" al registro

(define s6 (system-login s5 "user4")) ;se intenta loggear un usuario no registrado
(define s7 (system-login s6 "user1")) ;se loggea el user1
(define s8 (system-login s7 "user2")) ;el user2 no se puede loggear
(define s9 (system-logout s8)) ;se desloggea el user1
(define s10 (system-login s9 "user2")) ;se loggea el user2

(define s11 (system-talk-rec s10 "hola")) ;comienza la conversación
(define s12 (system-talk-rec s11 "jugar"))
(define s13 (system-talk-rec s12 "fortnite"))
(define s14 (system-talk-rec s13 "duo"))
(define s15 (system-talk-rec s14 "volver"))
(define s16 (system-talk-rec s14 "cantar"))
(display (system-synthesis s16 "user2")) ;se muestra la conversación entre el user2 y el chatbot
(display "\n")
(display "\n")
(display "\n")
(define s17 (system-logout s16)) ;se desloggea el user2
(define s18 (system-login s17 "user1")) ;se loggea el user1
(define s19 (system-talk-rec s18 "entrenar")) ;conversa
(define s20 (system-talk-rec s19 "correr"))
(display (system-synthesis s20 "user1")) ;se muestra la conversación del user1
