#lang eopl
(define scanner-spec-simple-interpreter
'((white-sp (whitespace) skip)
  (comment  ("%" (arbno (not #\newline))) skip)
  (identifier ("@" (arbno(or letter digit))) symbol)
  (text  (letter (arbno (or letter digit "?"))) string)
  (number  (digit (arbno (or digit "." )) )number)
  (number ("-" digit (arbno (or digit "." )) )number)
  ))


;especificacion sintactica (gramatica)
#|
- <programa> := <expresion>
                un-programa(exp)
- <expresion := <numero>
                numero-lit(num)
             := "\"" <texto> "\""
                texto-lit(txt)
             := <identificador>
                var-exp(id)
             := (expresion <primitiva-binaria> expresion)
                primapp-bin-exp (exp1 primBin exp2)
              := <primitiva-unaria> (expresion)
                 primapp-un-exp (primUn exp)
             := Si <expresion> entonces <expresion>  sino <expresion> finSI
                condicional-exp (test-exp true-exp false-exp)
            
             := declarar (<identificador> = <expresion> (;)) { <expresion> }
                variableLocal-exp (ids exps cuerpo)
             := procedimiento (<identificador>*',') haga <expresion> finProc
                procedimiento-ex (ids cuerpo)
             := "evaluar" expresion (expresion ",")*  finEval
                app-exp (id args)
<primitiva-binaria> :=  + (primitiva-suma)
                    :=  ~ (primitiva-resta)
                    :=  / (primitiva-div)
                    :=  * (primitiva-multi)
                    :=  concat (primitiva-concat)
<primitiva-unaria> :=  longitud (primitiva-longitud)
                   :=  add1 (primitiva-add1)
                   :=  sub1 (primitiva-sub1)
|#
(define grammar-simple-interpreter
  '(
    (program (expression) a-program)
    (expression (number) lit-exp)
    (expression ("\"" text "\"") lit-text)
    (expression (identifier) var-exp)
    (expression ( "(" expression primitiva-binaria expression ")") primapp-bin-exp)
    (expression (primitiva-unaria "(" expression ")") primapp-un-exp)
    (expression ("Si" expression "entonces" expression "sino" expression "finSI") if-exp)
    (expression
     ("declarar" "(" (separated-list identifier "=" expression ";") ")"
                 "{" expression "}") let-exp)
  
    
    (expression
     ("procedimiento" "(" (separated-list identifier ";") ")" "haga" expression "finProc") procedimiento-ex)

    
    (expression ("evaluar" expression  "(" (separated-list expression ",") ")" "finEval") eval-exp)

 (expression ("recursivo" "(" (arbno identifier ")" "(" (separated-list identifier ",") ")" "=" expression )  "{" expression "}")
              recur-exp)

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/" ) primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1" ) primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    )
  )


;Construidos automáticamente:
;;datatypes

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))
;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;; Ambiente inicial.
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e )
     '(1 2 3 "hola" "FLP")
     (empty-env))))
;*******************************************************************************************
;El Interprete
;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (lit-text (texto) texto)
      (var-exp (id) (apply-env env id))
      (primapp-bin-exp (rand1 prim rand2)
                  (let ((arg1 (eval-rand rand1 env)))
                    (let ((arg2 (eval-rand rand2 env)))
                      (apply-primitive prim arg1 arg2)
                      )
                    ))
      (primapp-un-exp (primUn rand)
                      (let ((arg (eval-rand rand env)))
                      (apply-primitive-un primUn arg)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
     (let-exp (ids rands body)
              (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))

      
      
      (procedimiento-ex (ids cuerpo) (cerradura ids cuerpo env))
      (eval-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (recur-exp (procs idss bodies principalBody)
                 (eval-expression principalBody
                              (extend-env-recursively procs idss bodies env)))
       
      )
    ))

;*********************
; Representacion de la cerradura para un procedimiento valido
;
; <cerradura>  := <(identificador)*> <expresion> <environment>
;
; Una cerradura guarda los componentes asociados a un procedimiento valido:
; Una lista de identificadores, una expresion y un ambiente 
;


;(define find-variable
;(lambda(env id)
 ;(apply-env env id)
;))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))


;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim arg1 arg2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ arg1 arg2 ))
      (primitiva-resta () (- arg1 arg2 ))
      (primitiva-multi () (* arg1 arg2 ) )
      (primitiva-div () (/ arg1 arg2 ))
      (primitiva-concat () (string-append arg1 arg2))
      )))
        
(define apply-primitive-un
  (lambda (prim args)
    (cases primitiva-unaria prim
          (primitiva-add1 () (+  args 1))
          (primitiva-sub1 () (- args 1))
          (primitiva-longitud (string-length (list->string '(args)))))
          )
        )

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))
;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (cerradura
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (cerradura (ids body env)
               (eval-expression body (extend-env ids args env))))))
;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))

                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

     (define (append-test lhs rhs)
  (if (and (string? lhs) (string? rhs))
      rhs
      (list lhs rhs)))
 
 ;PUNTO a) RADIO CIRCULO:
 (scan&parse "declarar (@radio=2.5;@areaCirculo=procedimiento (@x) haga ((@x*@x)*3.14) finProc) {  evaluar @areaCirculo (@radio) finEval  }")     
;PUNTO b) Factorial de 5 y 10
(scan&parse " declarar (@x=1; @a=procedimiento (@x) haga recursivo ( @fact ) (@x) = Si @x entonces (@x * evaluar @fact(sub1(@x)) finEval) sino 1 finSI  { evaluar @fact(@x) finEval} finProc)   {evaluar @a(5) finEval}")
(scan&parse " declarar (@x=1; @a=procedimiento (@x) haga recursivo ( @fact ) (@x) = Si @x entonces (@x * evaluar @fact(sub1(@x)) finEval) sino 1 finSI  { evaluar @fact(@x) finEval} finProc)   {evaluar @a(10) finEval}")
;Punto d) @Resta:
(scan&parse "declarar (@resta=procedimiento (@x;@y) haga recursivo ( @rest )(@x,@y) = Si @y entonces sub1(evaluar @rest(@x,sub1(@y)) finEval)sino @x finSI { evaluar @rest(@x,@y)finEval} finProc)  {evaluar @resta(10,3) finEval}")
(interpretador)
