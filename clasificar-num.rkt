#lang racket
; Función recursiva para clasificar números como Positivo, Negativo o Cero
(define (clasificar-num lst)
  (if (null? lst)
      '()
      (cons (cond [(> (car lst) 0) "Positivo"]
                  [(< (car lst) 0) "Negativo"]
                  [else "Cero"])
            (clasificar-num (cdr lst)))))

; Pruebas
(displayln (clasificar-num '(10 -5 0 7)))