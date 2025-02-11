#lang plai
(require math)
;; LENGUAJES DE PROGRAMACION 2025-2
;; PRACTICA 1
;; León García Gael Arturo   321333927
:: Diaz Payne Gabriel        317097560



;; Ejercicio 1
(define (parentesis-balanceados? s)
  (define result
    (for/fold ([contador 0]) ([c (in-string s)])
      (cond
        [(char=? c #\() (+ contador 1)]
        [(char=? c #\)) (if (> contador 0) (- contador 1) -1)]
        [else contador])))  
  (= result 0))  

;; Ejercicio 2

;; Función auxiliar para contar vocales en una cadena
(define (contar-vocales s)
  (define vocales '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
  (if (null? s)
      0
      (+ (if (member (car s) vocales) 1 0)
         (contar-vocales (cdr s)))))

; Función auxiliar para contar consonantes en una cadena
(define (contar-consonantes s)
  (define letras "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (define vocales "aeiouAEIOU")
  (if (null? s)
      0
      (+ (if (and (member (car s) (string->list letras))
                  (not (member (car s) (string->list vocales))))
             1
             0)
         (contar-consonantes (cdr s)))))

; Predicado que determina si hay más vocales que consonantes
(define (vocal-predominante? s)
  (> (contar-vocales (string->list s))
     (contar-consonantes (string->list s))))

; Ejercicio3
;;cadena-isomorfa? : String String -> Boolean
;; nota1: string-ref : regresa el carácter en la posición k de la cadena
(define (cadena-isomorfa? s1 s2)
    (if (not (= (string-length s1)(string-length s2))) #f
        (aux-cadena-iso s1 s2 '() '() 0)))

(define (aux-cadena-iso s1 s2 aux1 aux2 i)
  (if (= i (string-length s1))
      #t
      (let* ([char1 (string-ref s1 i)]
             [char2 (string-ref s2 i)]
             [relacion1 (assoc char1 aux1)]
             [relacion2 (assoc char2 aux2)])
        (if relacion1
            (if relacion2
                (if (equal? (cdr relacion1) char2)
                    (aux-cadena-iso s1 s2 relacion1 relacion2 ( + i 1))
                    #f)
                #f)
            (if relacion2
                #f
                (aux-cadena-iso s1 s2
                                (cons (cons char1 char2) aux1)
                                (cons (cons char2 char1) aux2)
                                (+ i 1)))))))


;; Ejercicio 4
; Predicado que recibe un numero entero y devuelve verdadero si es automórfico
(define (automorfico? n)
  (let* ((n2 (* n n)))  
    (if (= (modulo n2 10) n)
        #t
        #f)))

;; Ejercicio 5
; Función recursiva para clasificar números como Positivo, Negativo o Cero
(define (clasificar-num lst)
  (if (null? lst)
      '()
      (cons (cond [(> (car lst) 0) "Positivo"]
                  [(< (car lst) 0) "Negativo"]
                  [else "Cero"])
            (clasificar-num (cdr lst)))))

; Ejercicio 6
;;simulador-dado : number -> (listof number)
;; version en la que el número n está dentro del rango [1-6] y la lista se detiene cuando la simulacion cae en n
(define (simulador-dado n)
  (define num (+ 1 (random 6)))
  (if (not (= n num ))
      (cons num (simulador-dado n))
      (list num)))
;;version que se pide en la tarea
(define (simulador-dado-tarea n)
  (define dado (random 1 7))
  (if (>= dado n)
      (list dado)
      (cons dado (simulador-dado-tarea (- n dado)))))


;; Ejercicio 7
; Funcion recursiva que rota una lista a la izquierda un número de veces dado, recibe una lista y un número para rotar, devuelve la lista ya rotada.
(define (rotate-list lst n)
  (if (zero? n)
      lst
      (rotate-list (append (cdr lst) (list (car lst))) (- n 1))))

;; Ejercicio 8
; Función auxiliar recursiva que lleva un acumulador y un indicador de suma/resta
(define (zigzag-sum-helper lst signo)
  (if (null? lst)
      0
      (+ (* signo (car lst)) (zigzag-sum-helper (cdr lst) (- signo)))))

; Función principal que inicia el cálculo con signo positivo
(define (zigzag-sum lst)
  (zigzag-sum-helper lst 1))

; Ejercicio 9
;; generate-brackets : number -> (listof string)
(define (generate-brackets n)
  (generate-brackets-helper n n "")) ;

;; generate-brackets-helper : number number string -> (listof string)
(define (generate-brackets-helper izq der str)
  (cond
    [(and (= izq 0) (= der 0)) (list str)] ; 
    [else
     (append
      (if (> izq 0)
          (generate-brackets-helper (- izq 1) der (string-append str "(")) ; 
          '())
      (if (> der izq)
          (generate-brackets-helper izq (- der 1) (string-append str ")")) ;
          '()))]))

; Ejercicio 10 
;; contar-pasos-uno : number -> number
(define (contar-pasos-uno n)
  (define x 0)
  (cond
    [(= n 1) x]
    [else
     (define aux (if (= (modulo n 2) 0)
                     (/ n 2 )
                     (+ (* 3 n) 1)))
     (+ 1 (contar-pasos-uno aux))]))
