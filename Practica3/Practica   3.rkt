#lang plai

;; Definición de tipos
(define-type Binding
  [binding (id symbol?) (value FWAE?)])

(define-type FWAE
  [num (n number?)]
  [id (i symbol?)]
  [op (f symbol?) (args (listof FWAE?))]
  [with (bindings (listof Binding?)) (body FWAE?)]
  [with* (bindings (listof Binding?)) (body FWAE?)]
  [fun (params (listof symbol?)) (body FWAE?)]
  [app (fn FWAE?) (args (listof FWAE?))])

;; Función auxiliar para parsear un binding
(define (parse-binding b)
  (if (and (list? b) 
           (= (length b) 2) 
           (symbol? (first b)))
      (binding (first b) (parse (second b)))
      (error 'parse "Syntax Error: Binding mal formado")))

;; Función auxiliar para parsear una lista de bindings
(define (parse-bindings bindings)
  (map parse-binding bindings))

;; Parser
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    
    [(list? sexp)
     (case (first sexp)
       [(add1 sub1 + - * / = modulo expt)
        (op (first sexp) (map parse (rest sexp)))]
       
       ;; Caso with
       [(with)
        (if (and (>= (length sexp) 3) (list? (second sexp)))
            (with (parse-bindings (second sexp))
                  (parse (third sexp)))
            (error 'parse "Syntax Error: Expresión mal formada en with"))]
       
       ;; Caso with*
       [(with*)
        (if (and (>= (length sexp) 3) (list? (second sexp)))
            (with* (parse-bindings (second sexp))
                   (parse (third sexp)))
            (error 'parse "Syntax Error: Expresión mal formada en with*"))]
       
       ;; Caso fun
       [(fun)
        (if (and (= (length sexp) 3) (list? (second sexp)) (andmap symbol? (second sexp)))
            (fun (second sexp) 
                 (parse (third sexp)))
            (error 'parse "Syntax Error: Sintaxis inválida de fun"))]
       
       ;; Aplicación de función
       [else (app (parse (first sexp)) 
                  (map parse (rest sexp)))])]
    
    [else (error 'parse "Syntax Error: Expresión inválida: ~a" sexp)]))