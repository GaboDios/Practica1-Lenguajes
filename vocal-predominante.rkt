#lang racket

; Función auxiliar para contar vocales en una cadena
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

; Pruebas
(displayln (vocal-predominante? "abcdef"))    ; #f
(displayln (vocal-predominante? "aeiouaebcdfgh")) ; #t
