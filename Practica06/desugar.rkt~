#lang plai
(require (file "./grammars.rkt"))
;;(require (file "./parser.rkt"))

;; Función auxiliar: extrae los ids de los bindings
(define (extract-ids bindings)
  (map binding-id bindings))

;; Función auxiliar: desazucara el valor de un binding
(define (desugar-binding-value b)
  (desugar (binding-value b)))

;; Función auxiliar: extrae los valores desazucarados de los bindings
(define (desugar-values bindings)
  (map desugar-binding-value bindings))

;; Función auxiliar: desazucarar with*
(define (desugar-with* bindings body)
  (if (null? bindings)
      (desugar body)
      (let ([first-b (first bindings)]
            [rest-b (rest bindings)])
        (app (fun (list (binding-id first-b))
                  (desugar-with* rest-b body))
             (list (desugar (binding-value first-b)))))))

;; Función auxiliar: desazucarar condS en if anidados
(define (desugar-cond cases)
  (type-case Condition (first cases)
    [else-cond (else-expr)
      (desugar else-expr)]
    [condition (test then)
      (iF (desugar test)
          (desugar then)
          (desugar-cond (rest cases)))]))

;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
(define (desugar sexpr)
  (type-case SCFWBAE sexpr
    [numS (n) (num n)]
    [boolS (b) (bool b)]
    [idS (i) (id i)]

    ;; Nuevos casos
    [charS (c) (char c)]
    [stringS (s) (string s)]
    [listS (l) (list* (map desugar l))]

    [iFS (condicion then else)
         (iF (desugar condicion) (desugar then) (desugar else))]
    [opS (f args)
         (op f (map desugar args))]
    [funS (params body)
         (fun params (desugar body))]
    [appS (fun args)
         (app (desugar fun) (map desugar args))]

    [withS (bindings body)
     (app (fun (extract-ids bindings)
               (desugar body))
          (desugar-values bindings))]

    [withS* (bindings body)
     (desugar-with* bindings body)]

    [condS (cases)
     (desugar-cond cases)]))

