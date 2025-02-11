#lang racket

; Función auxiliar recursiva que lleva un acumulador y un indicador de suma/resta
(define (zigzag-sum-helper lst signo)
  (if (null? lst)
      0
      (+ (* signo (car lst)) (zigzag-sum-helper (cdr lst) (- signo)))))

; Función principal que inicia el cálculo con signo positivo
(define (zigzag-sum lst)
  (zigzag-sum-helper lst 1))

; Pruebas
(displayln (zigzag-sum '(1 2 3 4 5))) ; 1 - 2 + 3 - 4 + 5 = 3
(displayln (zigzag-sum '(10 20 30))) ; 10 - 20 + 30 = 20
(displayln (zigzag-sum '(5 5 5 5 5))) ; 5 - 5 + 5 - 5 + 5 = 5
