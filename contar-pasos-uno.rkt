#lang racket

; Función recursiva para contar los pasos hasta reducir n a 1
(define (contar-pasos-uno n)
  (cond
    [(= n 1) 0] ; Caso base: si n es 1, terminamos y devolvemos 0 pasos
    [(zero? (modulo n 2)) (+ 1 (contar-pasos-uno (/ n 2)))] ; Si es par, dividimos por 2
    [else (+ 1 (contar-pasos-uno (+ (* 3 n) 1)))])) ; Si es impar, aplicamos 3n + 1
; Pruebas
(displayln (contar-pasos-uno 6)) 
(displayln (contar-pasos-uno 15)) 
(displayln (contar-pasos-uno 1))  
(displayln (contar-pasos-uno 27)) 
