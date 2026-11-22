#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

#|
  Taller 3 — Cauca con referencias y tipos.
  Evaluación y punto de entrada.

  evaluar-expresion recorre el árbol con cases y apply-procedure liga
  los parámetros de una clausura antes de evaluar su cuerpo. Las dos se
  llaman mutuamente, así que comparten módulo, y con ellas la copia por
  valor de los argumentos, que describe cómo entra un valor a apply y no
  cómo está hecha la clausura.

  El evaluador ignora las anotaciones de tipo: las lee el parser, quedan
  en el árbol y solo las usa el chequeador.

  Las cláusulas de newref-exp, deref-exp, setref-exp, swap-exp y
  begin-exp van en evaluar-expresion, junto a las demás variantes.
  Las tres que verifican con referencia? antes de tocar el almacén son
  deref-exp, setref-exp y swap-exp; si el valor no es una referencia,
  levantan eopl:error.
|#

(require "sintaxis.rkt")
(require "almacen.rkt")
(require "ambiente.rkt")
(require "primitivas.rkt")

;; copia-por-valor : valor -> valor
;; El paso por valor entrega una copia del argumento. Para un número, un
;; booleano o un procedimiento la copia es el mismo valor; para una
;; referencia es una celda nueva con el mismo contenido. applyref no usa
;; esta función: ahí el parámetro queda ligado a la misma celda.
(define copia-por-valor
  (lambda (val)
    (if (referencia? val)
        (newref! (deref val))
        val)))

;; apply-procedure : procval x lista de valores -> valor
;; Verifica la aridad y evalúa el cuerpo en el ambiente de la clausura
;; extendido con los parámetros.
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (if (= (length ids) (length args))
                   (evaluar-expresion body (ambiente-extendido ids args env))
                   (eopl:error 'apply-procedure
                               "El procedimiento espera ~s argumentos y recibio ~s"
                               (length ids) (length args)))))))

;; evaluar-rands : lista de expresion x ambiente -> lista de valores
;; Evalúa las expresiones de izquierda a derecha.
(define evaluar-rands
  (lambda (rands env)
    (if (null? rands)
        '()
        (let ((primero (evaluar-expresion (car rands) env)))
          (cons primero (evaluar-rands (cdr rands) env))))))

;; evaluar-secuencia : valor x lista de expresion x ambiente -> valor
;; Evalúa las expresiones restantes de izquierda a derecha y retorna el
;; valor de la última; si no quedan, retorna el valor acumulado.
(define evaluar-secuencia
  (lambda (ultimo exps env)
    (if (null? exps)
        ultimo
        (evaluar-secuencia (evaluar-expresion (car exps) env) (cdr exps) env))))

;; elegir-rama : numero x lista de numero x lista de expresion x expresion
;;               x ambiente -> valor
;; Evalúa la rama del case cuyo literal coincide con el valor; si ninguna
;; coincide, evalúa la rama otherwise.
(define elegir-rama
  (lambda (val nums exps default env)
    (cond ((null? nums) (evaluar-expresion default env))
          ((= val (car nums)) (evaluar-expresion (car exps) env))
          (else (elegir-rama val (cdr nums) (cdr exps) default env)))))

;; exigir-referencia : symbol x valor -> referencia
;; Retorna el valor si es una referencia; si no, levanta eopl:error
;; nombrando la expresión que la exigía.
(define exigir-referencia
  (lambda (quien val)
    (if (referencia? val)
        val
        (eopl:error quien "Se esperaba una referencia y se encontro ~s" val))))

;; evaluar-ids-como-referencias : lista de symbol x ambiente -> lista de referencia
;; Busca cada identificador y verifica que esté ligado a una referencia.
;; Si alguno no lo está, levanta eopl:error.
(define evaluar-ids-como-referencias
  (lambda (ids env)
    (if (null? ids)
        '()
        (let ((val (apply-env env (car ids))))
          (if (referencia? val)
              (cons val (evaluar-ids-como-referencias (cdr ids) env))
              (eopl:error 'applyref
                          "El paso por referencia exige variables ligadas a referencias: ~s no lo esta"
                          (car ids)))))))

;; evaluar-expresion : expresion x ambiente -> valor
;; Una cláusula por variante de la gramática. La condición de un if se
;; verifica con boolean? antes de ramificar.
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (lit-exp (n) n)
      (var-exp (id) (apply-env env id))
      (true-exp () #t)
      (false-exp () #f)
      (prim-exp (prim rands)
                (evaluar-primitiva prim (evaluar-rands rands env)))
      (if-exp (test-exp true-branch false-branch)
              (let ((test (evaluar-expresion test-exp env)))
                (if (boolean? test)
                    (if test
                        (evaluar-expresion true-branch env)
                        (evaluar-expresion false-branch env))
                    (eopl:error 'if "La condicion debe ser booleana y se encontro ~s" test))))
      (let-exp (ids rands body)
               (evaluar-expresion body
                                  (ambiente-extendido ids (evaluar-rands rands env) env)))
      (proc-exp (ids tipos body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (evaluar-expresion rator env)))
                 (if (procval? proc)
                     (apply-procedure proc
                                      (map copia-por-valor (evaluar-rands rands env)))
                     (eopl:error 'apply "Se intento aplicar algo que no es un procedimiento: ~s"
                                 proc))))
      (letrec-exp (tipos-res proc-names idss tipos-args bodies letrec-body)
                  (evaluar-expresion letrec-body
                                     (ambiente-recursivo proc-names idss bodies env)))
      (case-exp (e nums exps default)
                (let ((val (evaluar-expresion e env)))
                  (if (number? val)
                      (elegir-rama val nums exps default env)
                      (eopl:error 'case "La expresion del case debe ser un numero y se encontro ~s"
                                  val))))
      (newref-exp (e)
                  (newref! (evaluar-expresion e env)))
      (deref-exp (e)
                 (let ((ref (evaluar-expresion e env)))
                   (deref (exigir-referencia 'deref ref))))
      (setref-exp (ref-exp val-exp)
                  (let* ((ref (evaluar-expresion ref-exp env))
                         (val (evaluar-expresion val-exp env)))
                    (setref! (exigir-referencia 'setref ref) val)
                    1))
      (swap-exp (ref1-exp ref2-exp)
                (let* ((ref1 (exigir-referencia 'swap (evaluar-expresion ref1-exp env)))
                       (ref2 (exigir-referencia 'swap (evaluar-expresion ref2-exp env))))
                  (let ((val1 (deref ref1))
                        (val2 (deref ref2)))
                    (setref! ref1 val2)
                    (setref! ref2 val1)
                    val2)))
      (begin-exp (first-exp rest-exps)
                 (evaluar-secuencia (evaluar-expresion first-exp env) rest-exps env))
      (app-ref-exp (rator ids)
                   (let ((proc (evaluar-expresion rator env)))
                     (if (procval? proc)
                         (apply-procedure proc (evaluar-ids-como-referencias ids env))
                         (eopl:error 'applyref
                                     "Se intento aplicar algo que no es un procedimiento: ~s"
                                     proc)))))))

;; evaluar-programa : programa -> valor
;; Deja el almacén vacío y evalúa el cuerpo en el ambiente inicial.
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-program (body)
                 (begin
                   (initialize-store!)
                   (evaluar-expresion body (ambiente-inicial)))))))

;; interpretar : string -> valor
;; Analiza el texto con el parser y lo evalúa.
(define interpretar
  (lambda (texto)
    (evaluar-programa (parser texto))))

;; interpretador : -> void
;; El ciclo de lectura, con sllgen:make-rep-loop sobre evaluar-programa.
;; make-rep-loop construye el ciclo y retorna un procedimiento sin
;; argumentos; el segundo par de paréntesis es el que lo ejecuta.
(define interpretador
  (lambda ()
    ((sllgen:make-rep-loop "--> "
                           evaluar-programa
                           (sllgen:make-stream-parser especificacion-lexica
                                                      especificacion-gramatical)))))

(provide (all-defined-out))