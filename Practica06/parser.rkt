#lang plai
(require (file "./grammars.rkt"))

;; parse: s-expression -> RCFWBAE-Typed
(define (parse sexp)
  (cond
    [(number? sexp) (numS sexp)]
    [(symbol? sexp) (idS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(char? sexp) (charS sexp)]
    [(string? sexp) (stringS sexp)]
    [(list? sexp)
     (case (first sexp)
       ;; List literal
       [(lst)
        (listS (map parse (rest sexp)))]

       ;; Operadores soportados
       [(+ - * / modulo expt add1 sub1 < <= = > >= not and or zero?
         num? bool? char? string? list?
         cons car cdr append length empty?
         string-append string-length)
        (opS (case (first sexp)
               [(+) +] [(-) -] [(*) *] [(/) /] [(modulo) modulo] [(expt) expt]
               [(add1) add1] [(sub1) sub1] [(<) <] [(<=) <=] [(=) =] [(>) >] [(>=) >=]
               [(not) not] [(and) (lambda (x y) (and x y))] [(or) (lambda (x y) (or x y))]
               [(zero?) zero?]
               [(num?) number?] [(bool?) boolean?] [(char?) char?] [(string?) string?] [(list?) list?]
               [(cons) cons] [(car) car] [(cdr) cdr] [(append) append] [(length) length]
               [(empty?) empty?] [(string-append) string-append] [(string-length) string-length]))
             (map parse (rest sexp))])]

       ;; if
       ['if
        (if (= (length sexp) 4)
            (iFS (parse (second sexp))
                 (parse (third sexp))
                 (parse (fourth sexp)))
            (error 'parse "Syntax Error: Expresión if mal formada"))]

       ;; if0
       ['if0
        (if (= (length sexp) 4)
            (iF0 (parse (second sexp))
                 (parse (third sexp))
                 (parse (fourth sexp)))
            (error 'parse "Syntax Error: Expresión if0 mal formada"))]

       ;; cond
       [(cond)
        (if (and (>= (length sexp) 2) (equal? (first (last sexp)) 'else))
            (condS (map parse-cond (rest sexp)))
            (error 'parse "Syntax Error: cond debe terminar con else"))]

       ;; with
       ['with
        (withS (parse-bindings (second sexp))
               (parse (third sexp)))]

       ;; with*
       ['with*
        (withS* (parse-bindings (second sexp))
                (parse (third sexp)))]

       ;; fun
       [(fun)
        (if (and (>= (length sexp) 5) (list? (second sexp)))
            (funS (map (lambda (p)
                         (param (first p) (parse-type (second p))))
                       (second sexp))
                  (parse-type (third sexp))
                  (parse (fourth sexp)))
            (error 'parse "Syntax Error: Expresión fun mal formada"))]

       ;; Aplicación general
       [else
        (appS (parse (first sexp))
              (map parse (rest sexp)))]))
   ;; [else (error 'parse "Syntax Error: Expresión inválida")]


;; parse-cond: s-expression -> Condition
(define (parse-cond c)
  (cond
    [(and (list? c) (equal? (first c) 'else))
     (else-cond (parse (second c)))]
    [(and (list? c) (>= (length c) 2))
     (condition (parse (first c)) (parse (second c)))]
    [else (error 'parse "Syntax Error: Condición mal formada")]))


;; parse-bindings: (listof s-expression) -> (listof bindingS)
(define (parse-bindings bindings)
  (map (lambda (b)
         (if (and (list? b) (= (length b) 3))
             (bindingS (first b) (parse-type (second b)) (parse (third b)))
             (error 'parse "Syntax Error: Binding mal formado")))
       bindings))


;; parse-type: s-expression -> Type
(define (parse-type t)
  (cond
    [(equal? t 'number) (numberT)]
    [(equal? t 'boolean) (booleanT)]
    [(equal? t 'char) (charT)]
    [(equal? t 'string) (stringT)]
    ;; Función: ( <tipo1> <tipo2> ... -> <tipo-result> )
    [(and (list? t) (member '-> t))
     (let* ([arrow-pos (ormap (lambda (i) (and (equal? (list-ref t i) '->) i)) (range (length t)))])
       (funT (map parse-type (take t arrow-pos))
             (parse-type (list-ref t (+ arrow-pos 1)))))
    ]
    [else (error 'parse-type "Tipo desconocido")]))
