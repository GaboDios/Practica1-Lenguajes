#lang plai
(require (file "./grammars.rkt"))

;; LENGUAJES DE PROGRAMACION 2025-2
;; PRACTICA 3
;; León García Gael Arturo   321333927
;; Diaz Payne Gabriel        317097560
;; Rojas Gutiérrez Ivana Fernanda 319095555


;; Toma una lista de números, símbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | <boolean>
;;    | listof(A)
;; parse: A -> SCFWBAE
;; parse: s-expression -> SCFWBAE
(define (parse sexp)
  (cond
    [(number? sexp) (numS sexp)]
    [(symbol? sexp) (idS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+ - * / modulo expt add1 sub1 < <= = > >= not and or zero?)
        (opS (case (first sexp)
               [(+) +] [(-) -] [(*) *] [(/) /]
               [(modulo) modulo] [(expt) expt]
               [(add1) add1] [(sub1) sub1]
               [(<) <] [(<=) <=] [(=) =] [(>) >] [(>=) >=]
               [(not) not] [(and) (lambda (x y) (and x y))]
               [(or) (lambda (x y) (or x y))] [(zero?) zero?])
             (map parse (rest sexp)))]
       [(if)
        (if (= (length sexp) 4)
            (iFS (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))
            (error 'parse "Syntax Error: Expresión if mal formada"))]
       [(cond)
        (if (and (>= (length sexp) 2) (equal? (first (last sexp)) 'else))
            (condS (map parse-cond (rest sexp)))
            (error 'parse "Syntax Error: cond debe terminar con else"))]
       [(with)
        (if (and (>= (length sexp) 3) (list? (second sexp)))
            (withS (parse-bindings (second sexp)) (parse (third sexp)))
            (error 'parse "Syntax Error: Expresión with mal formada"))]
       [(with*)
        (if (and (>= (length sexp) 3) (list? (second sexp)))
            (withS* (parse-bindings (second sexp)) (parse (third sexp)))
            (error 'parse "Syntax Error: Expresión with* mal formada"))]
       [(fun)
        (if (and (>= (length sexp) 3) (list? (second sexp)) (andmap symbol? (second sexp)))
            (funS (second sexp) (parse (third sexp)))
            (error 'parse "Syntax Error: Expresión fun mal formada"))]
       [else
        (appS (parse (first sexp)) (map parse (rest sexp)))])]
    [else (error 'parse "Syntax Error: Expresión inválida")]))

;; parse-cond: s-expression -> Condition
;; Parsea una condición para el cond
(define (parse-cond c)
  (cond
    [(and (list? c) (equal? (first c) 'else))
     (if (= (length c) 2)
         (else-cond (parse (second c)))
         (error 'parse "Syntax Error: else mal formado"))]
    [(and (list? c) (>= (length c) 2))
     (condition (parse (first c)) (parse (second c)))]
    [else (error 'parse "Syntax Error: Condición mal formada")]))

;; parse-binding: s-expression -> Binding
;; Parsea un binding individual
(define (parse-binding b)
  (if (and (list? b) (= (length b) 2) (symbol? (first b)))
      (binding (first b) (parse (second b)))
      (error 'parse "Syntax Error: Binding mal formado")))

;; parse-bindings: (listof s-expression) -> (listof Binding)
;; Parsea una lista de bindings
(define (parse-bindings bindings)
  (map parse-binding bindings))
