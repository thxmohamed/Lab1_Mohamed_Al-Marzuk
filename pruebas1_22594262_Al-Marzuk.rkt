#lang racket

(require "main_22594262_Al-Marzuk.rkt")

(define op1 (option  1 "1) Comer" 2 1 "hambre" "comer" "comida"))
(define op2 (option  2 "2) Trabajar" 3 1 "empleo" "trabajo" "trabajar"))

(define f10 (flow 1 "¡Hola! ¿Qué te gustaría hacer" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2 y op1
(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada

(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10

(define op3 (option  1 "1) Pollo Frito" 1 1 "pollo" "ave" "frito"))
(define op4 (option 2 "2) Hamburguesa" 1 1 "burger" "hamburguesa"))
(define op5 (option 3 "3) Papas Fritas" 1 1 "papas" "aceite" "mayonesa" "fritas"))
(define op6 (option 4 "4) No tengo hambre" 0 1 "volver" "no tengo hambre"))

(define op7 (option 1 "1) Conserje" 2 1 "conserje" "basura" "limpiar"))
(define op8 (option 2 "2) Taxista" 2 1 "taxi" "taxista" "conducir"))
(define op9 (option 3 "3) Camarero" 2 1 "camarero"))
(define op10 (option 4 "4) Ver más trabajos" 2 2 "más trabajo" "ver mas"))
(define op11 (option 5 "5) No quiero trabajar" 0 3 "no quiero trabajar" "atras"))

(define op12 (option 1 "1) Actor" 2 3 "actor" "actuacion" "teatro" "cine"))
(define op13 (option 2 "2) Cantante" 2 3 "cantar" "cantante"))
(define op14 (option 3 "3) Bailarín" 2 3 "bailar" "bailarin" "danzar" "moverse"))
(define op15 (option 4 "4) Futbolista" 2 3 "futbol" "futbolista" "messi" "cr7"))
(define op16 (option 5 "5) Trabajos anteriores" 2 1 "anteriores" "trabajos"))

(define op17 (option 1 "1) Profesional" 2 3 "profesional" "el mejor"))
(define op18 (option 2 "2) Experto" 2 3 "experto" "experimentado"))
(define op19 (option 3 "3) Principiante" 2 3 "principiante"))
(define op20 (option 4 "4) Amateur" 2 3 "amateur"))
(define op21 (option 5 "5) Otro trabajo" 2 2 "error" "otro"))

(define f20 (flow 1 "Flujo 1 Chatbot 1 \n¿Qué quieres comer?" op3 op4 op5 op6))
(define f30 (flow 1 "Flujo 1 Chatbot 1 \n¿De qué quieres trabajar?" op7 op8 op9 op10 op11))
(define f31 (flow 2 "Flujo 2 Chatbot 1 \n¿De qué quieres trabajar?" op12 op13 op14 op15 op16))
(define f32 (flow 3 "Flujo 3 Chatbot 1 \n¿Cuál es tu nivel en el trabajo?" op17 op18 op19 op20 op21))

(define cb1 (chatbot 1 "Chat de comida"  "Bienvenido\n¿Qué vas a comer?" 1 f20))
(define cb2 (chatbot 2 "Chat de trabajos"  "Bienvenido\n¿Qué vas a trabajar?" 1 f30 f31 f32))

(define s0 (system "Chatbots de prueba 2" 0 cb0 cb0 cb0 cb1 cb2))
(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2")) ;solo añade un ocurrencia de user2
(define s5 (system-add-user s4 "user3"))
(define s6 (system-login s5 "user8")) ;user8 no existe. No inicia sesión
(define s7 (system-login s6 "user1"))
(define s8 (system-login s7 "user2"))  ;no permite iniciar sesión a user2, pues user1 ya inició sesión
(define s9 (system-logout s8))
(define s10 (system-login s9 "user2"))

(define s11 (system-talk-rec s10 "hola")) ;comienza la conversación
(define s12 (system-talk-rec s11 "comer"))
(define s13 (system-talk-rec s12 "pollo"))
(display (system-synthesis s13 "user2")) ;se muestra la conversación entre el user2 y el chatbot
(display "\n")
(display "\n")
(display "\n")
(define s14 (system-logout s13)) ;se desloggea el user2
(define s15 (system-login s14 "user1")) ;se loggea el user1
(define s16 (system-talk-rec s15 "trabajo")) ;conversa
(define s17 (system-talk-rec s16 "ver mas"))
(define s18 (system-talk-rec s17 "futbol"))
(define s19 (system-talk-rec s18 "amateur"))
(display (system-synthesis s19 "user1")) ;se muestra la conversación del user1
