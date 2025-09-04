#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))
    

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> RCFWBAE-Typed
;; (define (lookup name ds)

;; Busca un identificador en el ambiente DefrdSub
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "Identificador libre: ~a" name)]
    [aSub (id val rest-env)
          (if (symbol=? id name)
              val
              (lookup name rest-env))]))

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: RCFWBAE-Typed DefrdSub-> RCFWBAE-Value
;;(define (interp expr ds) ... )
;; ==================================================
;; Funciones Auxiliares
;; ==================================================

;; Extiende el ambiente con nuevos parámetros
(define (extend-env names vals env)
  (cond
    [(empty? names) env]
    [else (aSub (first names)
                (first vals)
                (extend-env (rest names) (rest vals) env))]))

;; Operaciones con strings
(define (apply-string-op op arg-values)
  (cond
    [(equal? op string-append)
     (if (andmap stringV? arg-values)
         (stringV (apply string-append (map stringV-s arg-values)))
         (error "interp: argumentos no son strings para string-append"))]
    
    [(equal? op string-length)
     (if (and (= (length arg-values) 1) (stringV? (car arg-values)))
         (numV (string-length (stringV-s (car arg-values))))
         (error "interp: argumento inválido para string-length"))]
    [else (error "interp: operador de string desconocido")]))

;; Operaciones con listas (completa)
(define (apply-list-op op arg-values)
  (cond
    [(equal? op cons)
     (if (= (length arg-values) 2)
         (listV (cons (car arg-values) (listV-l (cadr arg-values))))
         (error "interp: aridad incorrecta para cons"))]
    
    [(equal? op car)
     (if (and (= (length arg-values) 1) 
              (listV? (car arg-values))
              (not (null? (listV-l (car arg-values)))))
         (car (listV-l (car arg-values)))
         (error "interp: argumento inválido para car"))]
    
    [(equal? op cdr)
     (if (and (= (length arg-values) 1)
              (listV? (car arg-values))
              (not (null? (listV-l (car arg-values)))))
         (listV (cdr (listV-l (car arg-values))))
         (error "interp: argumento inválido para cdr"))]
    
    [(equal? op append)
     (if (andmap listV? arg-values)
         (listV (apply append (map listV-l arg-values)))
         (error "interp: argumentos no son listas para append"))]
    
    [(equal? op length)
     (if (and (= (length arg-values) 1) (listV? (car arg-values)))
         (numV (length (listV-l (car arg-values))))
         (error "interp: argumento inválido para length"))]
    
    [(equal? op empty?)
     (boolV (or (null? arg-values)
               (and (listV? (car arg-values))
                    (null? (listV-l (car arg-values))))))]))

(define (interp expr ds)
  (type-case RCFWBAE-Typed expr
    [id (i) (lookup i ds)]
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [chaR (c) (charV c)]  ; Constructor corregido
    [strinG (s) (stringV s)]  ; Constructor corregido
    [lisT (elems) (listV (map (λ (e) (interp e ds)) elems))]  ; Constructor corregido

    [iF (cond-expr then-expr else-expr)
      (type-case RCFWBAE-Value (interp cond-expr ds)
        [boolV (b) (if b (interp then-expr ds) (interp else-expr ds))]
        [else (error "interp: condición no booleana")])]

    [op (f args)
      (let ([arg-values (map (λ (a) (interp a ds)) args)])
        (cond
          [(member f (list + - * / modulo expt add1 sub1))  ; Aritméticas
           (if (andmap numV? arg-values)
               (numV (apply f (map numV-n arg-values)))
               (error "interp: argumentos no numéricos"))]
          
          [(member f (list string-append string-length))  ; Strings
           (apply-string-op f arg-values)]
          
          [(member f (list cons car cdr append length empty?))  ; Listas
           (apply-list-op f arg-values)]
          
          [else (error "interp: operador desconocido")]))]

    [fun (params body)
      (closure params body ds)]

    [app (fun-expr arg-exprs)
      (type-case RCFWBAE-Value (interp fun-expr ds)
        [closure (params body env)
          (if (= (length params) (length arg-exprs))
              (interp body (extend-env params 
                                    (map (λ (a) (interp a ds)) arg-exprs)
                                    env))
              (error "interp: aridad incorrecta"))]
        [else (error "interp: no es una función")])]))