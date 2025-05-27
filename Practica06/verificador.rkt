#lang plai
(require (file "./grammars.rkt"))

;; ============================================================
;; Funciones auxiliares
;; ============================================================

;; Buscar el tipo asociado a un identificador en el contexto
(define (lookup name ctx)
  (type-case Type-Context ctx
    [phi () (error 'typeof (format "Variable ~a no definida" name))]
    [gamma (id tipo rest)
           (if (symbol=? id name)
               tipo
               (lookup name rest))]))

;; Verificar que todos los tipos en una lista sean iguales a uno dado
(define (check-all-same-type t types)
  (andmap (λ (x) (equal? t x)) types))

;; Obtener los tipos de los argumentos dados
(define (get-arg-types args ctx)
  (map (λ (arg) (typeof arg ctx)) args))

;; Agregar enlaces de `with` al contexto (evaluados en contexto actual)
(define (add-bindings bindings ctx)
  (foldl (λ (binding ctx)
           (let* ([id (bindingS-id binding)]
                  [declared-type (bindingS-type binding)]
                  [value (bindingS-value binding)]
                  [actual-type (typeof value ctx)])
             (if (equal? declared-type actual-type)
                 (gamma id declared-type ctx)
                 (error 'typeof (format "Tipo declarado no coincide para ~a" id)))))
         ctx
         bindings))


;; Agregar enlaces de `with*` al contexto (evaluados secuencialmente)
(define (add-bindings* bindings ctx)
  (if (empty? bindings)
      ctx
      (let* ([binding (first bindings)]
             [id (bindingS-id binding)]
             [declared-type (bindingS-type binding)]
             [value (bindingS-value binding)]
             [new-ctx (add-bindings* (rest bindings) ctx)]
             [actual-type (typeof value new-ctx)])
        (if (equal? declared-type actual-type)
            (gamma id declared-type new-ctx)
            (error 'typeof (format "Tipo declarado no coincide para ~a en with*" id))))))

;; Agregar parámetros de funciones al contexto
(define (add-params params ctx)
  (foldl (λ (param ctx)
           (gamma (param-param param) (param-tipo param) ctx))
         ctx
         params))

;; ============================================================
;; Función principal: typeof
;; ============================================================

(define (typeof expr ctx)
  (type-case SRCFWBAE-Typed expr
    [idS (i) (lookup i ctx)]
    [numS (_) (numberT)]
    [boolS (_) (booleanT)]
    [charS (_) (charT)]
    [stringS (_) (stringT)]
    ;;Dado que la gramática no incluye listT, las operaciones de lista tienen una verificación de tipos limitada. El verificador garantiza que las operaciones de lista tengan el número correcto de argumentos, pero no puede verificar completamente los tipos de los elementos de la lista.
    [listS (elems)
           (if (empty? elems)
               (error 'typeof "No se puede inferir tipo de lista vacía")
               (let ([first-type (typeof (first elems) ctx)])
                 (if (check-all-same-type first-type (map (λ (e) (typeof e ctx)) (rest elems)))
                     first-type
                     (error 'typeof "Elementos de lista tienen tipos diferentes"))))]

    [iF0 (cond then else)
         (let ([cond-type (typeof cond ctx)]
               [then-type (typeof then ctx)]
               [else-type (typeof else ctx)])
           (if (not (equal? cond-type (numberT)))
               (error 'typeof "Condición de if0 debe ser número")
               (if (equal? then-type else-type)
                   then-type
                   (error 'typeof "Ramas de if0 deben tener mismo tipo"))))]

    [iFS (cond then else)
         (let ([cond-type (typeof cond ctx)]
               [then-type (typeof then ctx)]
               [else-type (typeof else ctx)])
           (if (not (equal? cond-type (booleanT)))
               (error 'typeof "Condición de if debe ser booleano")
               (if (equal? then-type else-type)
                   then-type
                   (error 'typeof "Ramas de if deben tener mismo tipo"))))]

    [opS (f args)
         (let ([arg-types (get-arg-types args ctx)])
           (type-of-op f arg-types))]

    [condS (cases)
           (type-of-cond cases ctx)]

    [withS (bindings body)
           (typeof body (add-bindings bindings ctx))]

    [withS* (bindings body)
            (typeof body (add-bindings* bindings ctx))]

    [funS (params rtype body)
          (let* ([new-ctx (add-params params ctx)]
                 [body-type (typeof body new-ctx)])
            (if (equal? body-type rtype)
                (funT (map param-tipo params))
                (error 'typeof "Tipo de cuerpo no coincide con tipo declarado de retorno")))]

    [appS (fun args)
          (let ([fun-type (typeof fun ctx)]
                [arg-types (get-arg-types args ctx)])
            (type-case Type fun-type
              [funT (param-types)
                    (if (and (= (length param-types) (length arg-types))
                             (andmap equal? param-types arg-types))
                        ;; La gramática solo almacena tipos de parámetros, no tipos de retorno, por lo que la aplicación de función no verifica los tipos de retorno.
                        (error 'typeof "Aplicación de función necesita tipo de retorno en gramática")
                        (error 'typeof "Argumentos no coinciden con parámetros"))]
              [else (error 'typeof "Intento de aplicar no-función")]))]))

;; ============================================================
;; Funciones auxiliares de operaciones y condicionales
;; ============================================================

;; Verificación de tipos para operadores
(define (type-of-op op arg-types)
  (cond
    [(member op '(+ - * / modulo expt add1 sub1))
     (if (andmap (λ (t) (equal? t (numberT))) arg-types)
         (numberT)
         (error 'typeof "Operador numérico requiere argumentos numéricos"))]

    [(member op '(= < <= > >=))
     (if (and (andmap (λ (t) (equal? t (numberT))) arg-types)
              (= (length arg-types) 2))
         (booleanT)
         (error 'typeof "Operador de comparación requiere dos números"))]

    [(member op '(and or))
     (if (andmap (λ (t) (equal? t (booleanT))) arg-types)
         (booleanT)
         (error 'typeof "Operador lógico requiere argumentos booleanos"))]

    [(equal? op 'not)
     (if (and (= (length arg-types) 1)
              (equal? (first arg-types) (booleanT)))
         (booleanT)
         (error 'typeof "Operador not requiere un booleano"))]

    [(member op '(empty? list?))
     (if (= (length arg-types) 1)
         (booleanT)
         (error 'typeof "Operador de lista requiere un argumento"))]

    [(member op '(car cdr))
     (error 'typeof "car/cdr no soportados sin tipos de lista explícitos")]

    [(equal? op 'cons)
     (error 'typeof "cons no soportado sin tipos de lista explícitos")]

    [else (error 'typeof (format "Operador ~a no reconocido" op))]))

;; Verificación de tipos para expresiones condS
(define (type-of-cond cases ctx)
  (let ([types '()])
    (for-each 
     (λ (c)
       (type-case Condition c
         [condition (test then)
                    (let ([test-type (typeof test ctx)])
                      (if (not (equal? test-type (booleanT)))
                          (error 'typeof "Condición debe ser booleana")
                          (set! types (cons (typeof then ctx) types))))]
         [else-cond (else-expr)
                    (set! types (cons (typeof else-expr ctx) types))]))
     cases)
    (if (and (not (empty? types)) (check-all-same-type (first types) (rest types)))
        (first types)
        (error 'typeof "Ramas de cond deben tener mismo tipo"))))





