#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> CFWAE
;; parse: s-expression -> CFWAE

;; parse : s-expression - > CFWAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]   
    [(list? sexp)
     (case (first sexp)
       [(add1 sub1 + - * / = modulo expt)
        (op (case (first sexp)
             [(+) +]
             [(-) -]
             [(*) *]
             [(/) /]
             [(modulo) modulo]
             [(expt) expt]
             [(add1) add1]
             [(sub1) sub1]
             [else (error 'parse "Operador desconocido")])
          (map parse (rest sexp)))]
       [(with*)
        (if (and (>= (length sexp) 3) (list? (second sexp)))
            (with* (parse-bindings (second sexp))
                   (parse (third sexp)))
            (error 'parse "Syntax Error: Expresión mal formada en with*"))]
       [(if0)
        (if (= (length sexp) 4)
            (if0 (parse (second sexp))
                 (parse (third sexp))
                 (parse (fourth sexp)))
            (error 'parse "Syntax Error: Expresión mal formada en if0"))]
       [(fun)
        (if (and (= (length sexp) 3) 
                 (list? (second sexp)) 
                 (andmap symbol? (second sexp))) 
            (fun (second sexp) (parse (third sexp)))
            (error 'parse "Syntax Error: Expresión mal formada en fun"))]
       [else 
        (if (list? (first sexp))
            (app (parse (first sexp)) (map parse (rest sexp)))
            (error 'parse "Syntax Error: Expresión mal formada en aplicación de función"))])]

    [else (error 'parse "Syntax Error: Expresión inválida")]))

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
