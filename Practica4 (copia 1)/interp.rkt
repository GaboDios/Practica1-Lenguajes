#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWAE
;; (define (lookup name ds)

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error "Variable libre: " name)]
    [aSub (id val rest-ds)
          (if (symbol=? name id)
              val
              (lookup name rest-ds))]))

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWAE DefrdSub-> CFWAE-Value
(define (interp expr ds)
  (type-case CFWAE expr
    [id (i) (lookup i ds)]
    [num (n) (numV n)]
    [if0 (cond then else)
     (let ([cond-val (interp cond ds)])
       (if (numV? cond-val)
           (if (zero? (numV-n cond-val))
               (interp then ds)
               (interp else ds))
           (error "interp: Condición en if0 no es numérica")))]
    
    [op (f args)
     (let ([eval-args (map (λ (arg) (interp arg ds)) args)])
       (if (andmap numV? eval-args)
           (numV (apply f (map numV-n eval-args)))
           (error "interp: Argumentos no numéricos en op")))]
    
    [with* (bindings body)
     (interp body (extend-ds* bindings ds))]  
    
    [fun (params body)
     (closure params body ds)]  
    
    [app (fun-expr arg-exprs)
     (let ([fun-val (interp fun-expr ds)]
           [arg-vals (map (λ (arg) (interp arg ds)) arg-exprs)])
       (type-case CFWAE-Value fun-val
         [closure (params body env)
          (if (= (length params) (length arg-vals))
              (interp body (extend-ds params arg-vals env)) 
              (error "interp: Aridad incorrecta"))]
         [else (error "interp: No es una función")]))]))

(define (extend-ds params arg-vals ds)
  (if (null? params)
      ds
      (aSub (car params)
            (car arg-vals)
            (extend-ds (cdr params) (cdr arg-vals) ds))))

(define (extend-ds* bindings ds)
  (if (null? bindings)
      ds
      (let ([b (car bindings)])
        (aSub (binding-id b)
              (interp (binding-value b) ds)  
              (extend-ds* (cdr bindings) ds)))))
