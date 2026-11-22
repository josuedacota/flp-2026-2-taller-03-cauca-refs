#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

#|
  Taller 3 — Cauca con referencias y tipos.
  Primitivas del lenguaje.

  evaluar-primitiva recibe la primitiva del árbol y la lista de valores
  ya evaluados, revisa la cantidad y el tipo de los argumentos y produce
  el resultado. Solo necesita la gramática, no el evaluador.

  Las verificaciones de cantidad y de tipo van aquí, junto a la
  primitiva que las exige, y levantan eopl:error con un mensaje que
  nombre la primitiva.
|#

(require "sintaxis.rkt")

;; verificar-aridad : symbol x lista x integer -> boolean
;; Levanta eopl:error si la lista no tiene exactamente n elementos.
;; Retorna #t si la cantidad es correcta.
(define verificar-aridad
  (lambda (nombre args n)
    (if (= (length args) n)
        #t
        (eopl:error nombre "La primitiva espera ~s argumentos y recibio ~s"
                    n (length args)))))

;; verificar-numeros : symbol x lista -> boolean
;; Levanta eopl:error si algún argumento no es un número.
(define verificar-numeros
  (lambda (nombre args)
    (cond ((null? args) #t)
          ((number? (car args)) (verificar-numeros nombre (cdr args)))
          (else (eopl:error nombre "El argumento ~s no es un numero" (car args))))))

;; verificar-booleanos : symbol x lista -> boolean
;; Levanta eopl:error si algún argumento no es un booleano.
(define verificar-booleanos
  (lambda (nombre args)
    (cond ((null? args) #t)
          ((boolean? (car args)) (verificar-booleanos nombre (cdr args)))
          (else (eopl:error nombre "El argumento ~s no es un booleano" (car args))))))

;; aritmetica-binaria : symbol x (num x num -> num) x lista -> num
;; Verifica dos argumentos numéricos y aplica la operación.
(define aritmetica-binaria
  (lambda (nombre op args)
    (verificar-aridad nombre args 2)
    (verificar-numeros nombre args)
    (op (car args) (cadr args))))

;; aritmetica-unaria : symbol x (num -> num) x lista -> num
;; Verifica un argumento numérico y aplica la operación.
(define aritmetica-unaria
  (lambda (nombre op args)
    (verificar-aridad nombre args 1)
    (verificar-numeros nombre args)
    (op (car args))))

;; relacional : symbol x (num x num -> boolean) x lista -> boolean
;; Verifica dos argumentos numéricos y aplica la comparación.
(define relacional
  (lambda (nombre op args)
    (verificar-aridad nombre args 2)
    (verificar-numeros nombre args)
    (op (car args) (cadr args))))

;; evaluar-primitiva : primitiva x lista de valores -> valor
;; Una cláusula por primitiva de la gramática, con cases.
(define evaluar-primitiva
  (lambda (prim args)
    (cases primitiva prim
      (sum-prim () (aritmetica-binaria '+ + args))
      (minus-prim () (aritmetica-binaria '- - args))
      (mult-prim () (aritmetica-binaria '* * args))
      (div-prim ()
                (aritmetica-binaria '/
                                    (lambda (a b)
                                      (if (= b 0)
                                          (eopl:error '/ "Division por cero")
                                          (quotient a b)))
                                    args))
      (add1-prim () (aritmetica-unaria 'add1 (lambda (a) (+ a 1)) args))
      (sub1-prim () (aritmetica-unaria 'sub1 (lambda (a) (- a 1)) args))
      (less-prim () (relacional '< < args))
      (greater-prim () (relacional '> > args))
      (less-eq-prim () (relacional '<= <= args))
      (greater-eq-prim () (relacional '>= >= args))
      (equal-prim () (relacional '== = args))
      (not-equal-prim () (relacional '!= (lambda (a b) (not (= a b))) args))
      (and-prim ()
                (verificar-aridad 'and args 2)
                (verificar-booleanos 'and args)
                (and (car args) (cadr args)))
      (or-prim ()
               (verificar-aridad 'or args 2)
               (verificar-booleanos 'or args)
               (or (car args) (cadr args)))
      (not-prim ()
                (verificar-aridad 'not args 1)
                (verificar-booleanos 'not args)
                (not (car args)))
      ;; entre(x, lo, hi) es verdadero cuando lo <= x <= hi
      (entre-prim ()
                  (verificar-aridad 'entre args 3)
                  (verificar-numeros 'entre args)
                  (and (<= (cadr args) (car args))
                       (<= (car args) (caddr args)))))))

(provide (all-defined-out))
