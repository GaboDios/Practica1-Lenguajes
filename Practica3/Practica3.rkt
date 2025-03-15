#lang plai

;; LENGUAJES DE PROGRAMACION 2025-2
;; PRACTICA 3
;; León García Gael Arturo   321333927
;; Diaz Payne Gabriel        317097560
;; Rojas Gutiérrez Ivana Fernanda 319095555

;; Def del tipo Binding
(define-type Binding
  [binding (id symbol?) (value FWAE?)])

;; Def del tipo FWAE
(define-type FWAE
  [id (i symbol?)]
  [num (n number?)]
  [op (f procedure?)(args (listof FWAE?))]
  [with (bindings (listof binding?)) (body FWAE?)]
  [with* (bindings (listof binding?)) ( body FWAE?)])

;; subst : FWAE symbol FWAE - > FWAE
(define (subst expr sub-id value)
  (cond
    [(id? expr)
     (if (equal? (id-i expr) sub-id)
            value
            expr)]
    [(num? expr) expr]
    [(op? expr) (op (op-f expr) (map (lambda (arg) (subst arg sub-id value)) (op-args expr)))]
    [(with? expr)
     (with (map (lambda (b)
                  (binding (binding-id b)  .
                           (subst (binding-value b) sub-id value)))
            (with-bindings expr))
           (if (member sub-id (map binding-id (with-bindings expr)))
               (with-body expr)
               (subst (with-body expr) sub-id value)))]
    [(with*? expr)
     (with* (let loop ([bindings (with*-bindings expr)] [acc '()])
             (if (null? bindings)
                 (reverse acc)
                 (let* ([b (car bindings)]
                        [new-binding (binding (binding-id b)
                                              (subst (binding-value b) sub-id value))])
                   (loop (cdr bindings) (cons new-binding acc)))))
            (if (member sub-id (map binding-id (with*-bindings expr)))
                (with*-body expr)  
                (subst (with*-body expr) sub-id value)))]
  ))


;;Evaluador de expresiones FWAE
(define (interp expr)
  (type-case FWAE expr
    [id (i) (error "Variable libre")]
    [num (n) expr]
    [op (f args)
     (let ([eval-args (map interp args)])  ;; Evaluar los argumentos
       (num (apply f (map num-n eval-args))))]  ;; Aplicar la función f a los argumentos
    [with (bindings body)
     (interp (aplicar-bindings bindings body))]  ;; Evaluar el cuerpo en el ámbito de los bindings
    [with* (bindings body)
     (interp (aplicar-bindings* bindings body))]))
;;Funciones Auxiliares
(define (aplicar-bindings bindings body)
  (if (null? bindings)
      body
      (aplicar-bindings (cdr bindings) 
                        (subst body (binding-id (car bindings)) 
                                     (binding-value (car bindings))))))
(define (aplicar-bindings* bindings body)
  (if (null? bindings)
      body
      (aplicar-bindings* (cdr bindings) 
                         (subst body (binding-id (car bindings)) 
                                      (interp (binding-value (car bindings)))))))

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
        (op (case (first sexp)  ;; Mapear el símbolo a la función correspondiente
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
       
       )]
    
    [else (error 'parse "Syntax Error: Expresión inválida: ~a" sexp)]))
