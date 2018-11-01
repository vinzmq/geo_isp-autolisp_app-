
;  Определение  отклонений относительно наклонной линии с учётом импрвизации

; By Martynyuk Vyacheslav
( defun C:geo_isp ( / point A B C x0 y0 xA yA xB yB xP yP perpend an p3 p4 pointt  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rnd (/ modulus multiplier increment random)
  (if (not seed)
    (setq seed (getvar "DATE"))
  )
  (setq modulus    65536
        multiplier 25173
        increment  13849
        seed  (rem (+ (* multiplier seed) increment) modulus)
        random     (/ seed modulus)
  )
)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun GetInputNoErrHandling(/ returnPnt)
    ;; Define the valid keywords
    (initget 128 "Размерный_стиль")
    
    ;; Get the user input
    (setq returnPnt (getpoint "Факт или [Размерный_стиль]: "))

    (if (/= returnPnt nil)
        (cond
            ((= (type returnPnt) 'LIST)
               (setq point returnPnt)
            )
            ((= (type returnPnt) 'STR)
              (progn
                (setq Htext (rtos(getreal "\nВведите высоту текста: ")))
                (setq scal (getreal "\nВведите размер стрелки : "))
                (setq Dopusk (getint "\nВведите допустимое отклонение в мм: " ) )
		(setq impro (getint "\nВведите 1 для автоподбора отклонения или 0 для роботы в ручном режиме: "))
                (setq point (getpoint " \nФакт:"))
		
               )
            )        
        )
        (alert "No point was picked.")
    )
 
)



 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setvar "osmode" 15359 )

  (GetInputNoErrHandling)
  
  ;( setq point (getpoint " Факт" ))               ; Фактическое значение
  ( setq A (getpoint "\nНачальная точка линии " ) ) ; начальная точка линии
  ( setq B (getpoint "\nКонечная точка линии " ) )   ; конечная точка линии
  ( setq C (getpoint " \nРасположение текста отклонения [Высота_текста/Размер_стрелки]  ")  )

(if ( null scal) (setq scal 1) )
(if ( null Htext) (setq Htext 1) )
(if ( null Dopusk) (setq Dopusk 15) )
(if (null impro) (setq impro 2))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq xA (car A ) )   ; определение Х начальной точки линии
  (setq yA (cadr A ) )  ; определение Y начальной точки линии
  
  (setq xB (car B ) )    ; определение Х конечной точки линии
  (setq yB (cadr B ) )   ; определение Y конечной точки линии

  (setq xC (car C ) )    ; определение Х точки расположения стрелки
  (setq yC (cadr C ) )   ; определение Y точки расположения стрелки


  (setq xP (car point ) )     ; определение Х фактической точки 
  (setq yP (cadr point ) )    ; определение Y фактической точки 


  ; Определение координат перпендикуляра опущеного с точки на линию
  (if (= xA xB ) (progn (setq x0 xA) (setq y0 yP ))
  (if (= yA yB ) (progn (setq x0 xP ) (setq y0 yA))
(progn
   (setq x0 ( / (+ (* xA (* (- yB yA) (- yB yA))) (* xP (* (- xB xA ) (- xB xA ))) (* (- xB xA) (- yB yA ) (- yP yA)))  (+ (* (- yB yA) (- yB yA)) (* (- xB xA) (- xB xA)))))

  (setq y0  ( + (/ (* (- xB xA) (- xP x0 )) (- yB yA))  yP  ) )
  )))

( setq Dd ( - (* (- xC xA) (- yB yA) ) (* (- yC yA) (- xB xA)  ) ) )

( setq Dd2 ( - (* (- xP xA) (- yB yA) ) (* (- yP yA) (- xB xA)  ) ) )

  

 ;  точка перпендикуляр
 (setq perpend (list x0 y0) )

  (setq osm (getvar "osmode") )
  (setvar "osmode" 0 )
  
  ; Прорисовк линии от фактической точки к точке перпендикуляра
 (command "_line" perpend point "" )

;изьятие из последнего нарисованого примитива координат точек начала и конца

(setq edata (entget (entlast)) )
( setq qe   (assoc 10 edata ))
( setq qe2  (assoc 11 edata ))
(entdel (entlast) )
(setq qee ( cdr qe) )
(setq qee2 ( cdr qe2) )

(setq dr (list (car qee) (cadr qee ) ) )
(setq dr2 (list (car qee2) (cadr qee2) ) ) 

  
  ;  единичный отрезок
    (setq r (list 0 scal 0 ) )
  (setq s (list 0 0 0 ) )
(distance r s )
  

  ; рисование стрелки


; В зависимости от того в какую сторону будет находится стрелка  определяем ее координаты 
  (if (or (and (< Dd 0 ) (< Dd2 0)) (and (> Dd 0 ) (> Dd2 0)) )

(progn

  (setq an (angle  qee  qee2) ) ; направление линии
(setq pointt (polar perpend an (distance qee qee2) ) ) ; координаты точки отклонение = point 
(setq p3 (polar pointt (+ (- an (/ pi 6)) pi ) (/ (distance qee qee2)  3 ) ) ) ; определение координат стрелки 
(setq p4 (polar pointt ( + (+ an (/ pi 6)) pi ) (/ (distance qee qee2)  3 ) ) )

  
    (command "_line" perpend pointt "" ); рисование линии
  (command "_scale" (entlast) "" perpend (/ (distance r s ) (distance qee qee2)) ) ; масштабирование


  (command "_line" pointt p3 "" )
  (command "_scale" (entlast) "" perpend (/ (distance r s ) (distance qee qee2)) )
  
  (command "_line" pointt p4 "" )
  (command "_scale" (entlast) "" perpend (/ (distance r s ) (distance qee qee2)) )

)

  ( progn

  (setq an (angle  qee  qee2) ) ; направление линии
(setq pointtt (polar perpend ( + an pi) (distance qee qee2) ) ) ; координаты точки отклонение = point 
(setq p3 (polar perpend (+ (- an (/ pi 6)) pi ) (/ (distance qee qee2)  3 ) ) ) ; определение координат стрелки 
(setq p4 (polar perpend ( + (+ an (/ pi 6)) pi ) (/ (distance qee qee2)  3 ) ) )

    

  (command "_line" perpend pointtt "" ); рисование линии
  (command "_scale" (entlast) "" perpend (/ (distance r s ) (distance qee qee2)) ) ; масштабирование


  (command "_line" perpend p3 "" )
  (command "_scale" (entlast) "" perpend (/ (distance r s ) (distance qee qee2)) )

  
  (command "_line" perpend p4 "" )
  (command "_scale" (entlast) "" perpend (/ (distance r s ) (distance qee qee2)) )
  
    )
  )


 
(if (= impro 0) (progn

(if (< dopusk (* 1000 (distance dr dr2 )) )

 (progn

 (setq oldColor (getvar "CECOLOR"))
 (setvar "CECOLOR" "1") 
 (command "_text" "_j" "_bl" "_non" C Htext "0"  ( fix (* 1000 (distance dr dr2 ))))
 (setvar "CECOLOR" OldColor)
)
  (command "_text" "_j" "_bl" "_non" C Htext "0"  ( fix (* 1000 (distance dr dr2 ))))
 ); if color
);progn
(command "_text" "_j" "_bl" "_non" C Htext "0"  (fix(* dopusk (rnd))))
  )
 

 (setq qaz (entlast) )
 (command "_.rotate"  qaz "" C   )


     (setvar "osmode" osm )

  (gc)
  )

