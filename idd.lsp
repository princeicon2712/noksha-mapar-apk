;;
;; 좌표 추출하기
;;
(defun c:idd()

  (setq lup(getvar "luprec")) 
  (setvar "luprec" 3)
  (setq zin(getvar "dimzin"))
  (setvar "dimzin" 0)
  (setvar "cmdecho" 0)

  (setq po (getpoint "Pick First Point:"))
  (setq po2 (getpoint po "Pick Second Point:"))

(setq xp (strcat "N " (rtos (cadr po) 2 3)))
(setq yp (strcat "E " (rtos (car po) 2 3)))

(command "leader" po po2 "" yp xp "")
)
