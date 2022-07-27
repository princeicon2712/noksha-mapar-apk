;;; ROTINA PARA COTAR E OU AZIMUTAR POLYLINES - TEXTO NO DESENHO = 2.34 ou 2.34 - Az 23°15´7" 
;;; AS MEDIDAS APARECEM COMO FORAM DIGITADAS NO DESENHO (METROS, MILÍMETROS, ETC) 
;;; A ALTURA DO TEXTO É SOLICITADA NA EXECUÇÃO DA ROTINA E O STYLE É O CORRENTE 
;;; ACERTAR ALTURA DO TEXTO DE ACORDO COM A ESCALA DE PLOTAGEM. PARA DEIXAR EM DEFAULT USE O COMMAND: TEXTSIZE 
;;; Forum www.autolisp.com.br - Nome da rotina original: AZPOL - Autor: Orlei 29/08/2006 
;;; Adaptação: Rogério Zanini e Márcio 
;;;---------------------------------------------------------- ------------------------ 

(princ "\nDigite AZI para iniciar - Verificar TextStyle corrente") 
(defun C:AZI () 
(command "undo" "begin"); Voltar de uma só vez toda a operação da Rotina - ACRESCENTADO ROGÉRIO 
;---------------Sub-functions - Start------------- 
(defun RTD () 
(/ (* (angle A B) 180) Pi) 
) 
;------------------------------------------------- 
(defun DTR (AZIMUTE) 
(* (/ AZIMUTE 180) Pi) 
) 
;------------------------------------------------- 
(defun PARALELO () 
(setq A1 (polar A (+ (/ pi 2)(angle B A )) (+ (* 0.60 htext) (/ htext 2)))) ;0.60 x ALTURA TEXTO = distância do texto da Pline ;MODIFICAÇÃO ROGÉRIO 
(setq B1 (polar B (+ (/ pi 2)(angle B A )) (+ (* 0.60 htext) (/ htext 2)))) ;0.60 x ALTURA TEXTO = distância do texto da Pline ;MODIFICAÇÃO ROGÉRIO 

(setq ptx (/ (+ (car B1) (car A1)) 2)) 
(setq pty (/ (+ (cadr B1) (cadr A1)) 2)) 
(setq ponto_meio (list ptx pty)) 
(if (< (car A1)(car B1)) 
(setq inicio B1) 
(setq inicio A1) 
) 
) 
;------------------------------------------------- 
(defun MUDAR () 
(setq XL 2) 
(setq J "d") 
(setq COM1 (substr D 1 1)) 
(while (< XL 5) 
(setq LETRAT (substr D XL 1)) 
(setq RESTOT (substr D (+ 1 XL) )) 
(if (= LETRAT J) 
(progn (setq J "%%d") 
(setq XL 6) 
(setq PALAV (strcat COM1 J RESTOT)) 
) 
) 
(setq COM1 (strcat COM1 LETRAT )) 
(setq XL (1+ XL)) 
) 
) 
;----------------Sub-functions - End ------------- 
(setq tsize (getvar "textsize")) 
(initget "Y N") 
(setq opt (getkword "\nIncluir Dados de Azimute? [Y/N] <N>: ")) 

(if (= nil 
(setq htext (getreal (strcat "\nAltura do texto <" (rtos (getvar "textsize") 2 2) ">: "))) 
) 
(setq htext (getvar "textsize"))) 

(setvar"cmdecho" 0) 
(command "osmode" 0) 
(command "angbase" 270) 
(command "angdir" 1) 
(setq flagv "falso") 
(setq controle 0) 
(setq controle1 0) 
(setq contador 0) 
(while (= flagv "falso") 
(setq mostre (entsel "\nSelecione a Polyline <2d> : ")) 
(setq linha (entget (car mostre ))) 
(setq verificador (cdr(assoc 0 linha))) 
(if (= verificador "LWPOLYLINE") 
(progn 
(setq verif (cdr (assoc 70 linha))) 
(setq flagv "verdade") 
) 
(princ "\nNão é Polyline !! ") 
) 
) 

(setq controle1 (length linha)) 
(setq amostra '()) 
(repeat controle1 
(setq x (caar linha)) 
(if (= x 10) 
(progn 
(setq item (car linha)) 
(setq amostra (cons item amostra)) 
(setq contador (1+ contador)) 
) 
) 
(setq linha (cdr linha)) 
) 
(setq amostra1 (reverse amostra)) 
(if (= verif 1) 
(setq amostra (cons (car amostra1) amostra)) 
(setq contador (1- contador)) 
) 
(setq controle contador) 
(repeat controle 
(setq PTO1 (cdr(car amostra))) 
(setq PTO2 (cdr(car(cdr amostra)))) 
(AZIMUTAR) 
(setq amostra(cdr amostra)) 
) 
(command "undo" "end") 
(princ) 
) 
(defun AZIMUTAR () 
(setq padroes (getvar "osmode")) 
(setvar"cmdecho" 0) 
(command "osmode" 0) 
(setq A PTO1) 
(setq B PTO2) 
(setq C " - Az "); PREFIXO DO ÂNGULO OU "m - Az " m=para metros OU mm=para milímetros 
(setq D (angtos (angle A B) 1 4)) ;UNIDADE/N°DE CASAS 1= Decimal degrees = 45.0000 / 4= N° CASAS = 270°0'0" 
(MUDAR) 
;------------- MODIFICAÇÃO ROGÉRIO OPÇÃO AZIMUTE ---------------- 
(setq E (Strcat(rtos (distance A B) 2 2)"m"))  ;PRIMEIRO 2= UNIDADE DECIMAL / SEGUNDO 2= N°DE CASAS 
(setq DADO E) 
(if (= opt "Y") (setq DADO (strcat E C PALAV))) 
;---------------------------------------------------------- ------ 
(PARALELO) 
;;; (command "text" "J" "MC" ponto_meio htext inicio dado ) ;htext = ALTURA TEXTO ;MODIFICAÇÃO ROGÉRIO 
(entmake 
(list 
(cons 0 "TEXT") 
(cons 7 (getvar "textstyle")) ;STYLE DE TEXTO CORRENTE DO DESENHO 
; (cons 8 "0") ;ATIVADO A COTA FICA NA LAYER "0" OU OUTRA INDICADA 
(cons 100 "AcDbText") 
(cons 10 ponto_meio) 
(cons 11 ponto_meio) 
(cons 40 htext) ;ALTURA DO TEXTO CORRENTE NO DESENHO 
(cons 1 dado) 
(cons 50 (angle ponto_meio inicio)) ;TEXTO PARALELO A LINHA 
(cons 72 1) ;JUSTIFICAÇÃO TEXTO HORIZONTAL - CENTER 
(cons 73 2) ;JUSTIFICAÇÃO TEXTO VERTICAL - MIDDLE 
) 
) 

(setvar "textsize" tsize) 
(command "osmode" padroes) 
(command "osmode" "37") ; ACRESCENTADO MÁRCIO - OSNAP END/INT/CEN 
(princ) 
) 