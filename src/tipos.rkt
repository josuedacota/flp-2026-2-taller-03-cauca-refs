#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

#|
  Taller 3 — Cauca con referencias y tipos.
  El tipo y el ambiente de tipos (Parte 3).

  El datatype tipo lo genera la gramática, porque las anotaciones se
  escriben dentro del programa. Aquí van la comparación estructural de
  dos tipos, el texto con que un tipo aparece en los mensajes de error y
  el ambiente de tipos.

  Falta el datatype del ambiente de tipos, con las mismas tres variantes
  del ambiente de valores: tenv-vacio, tenv-extendido y tenv-recursivo.
  La recursiva guarda el tipo declarado de cada procedimiento, que es lo
  que permite chequear el cuerpo de un letrec sin evaluarlo.

  El módulo requiere sintaxis.rkt y ningún otro del intérprete.
|#

(require "sintaxis.rkt")

;; unir-textos : lista de string x string -> string
;; Une los textos de la lista poniendo el separador entre cada par.
;; La lista vacía da la cadena vacía.
(define unir-textos
  (lambda (textos separador)
    (cond ((null? textos) "")
          ((null? (cdr textos)) (car textos))
          (else (string-append (car textos) separador
                               (unir-textos (cdr textos) separador))))))

;; tipo->texto : tipo -> string
;; La forma en que los tipos aparecen en los mensajes de error:
;; "num", "bool", "refto(num)", "(num * num -> bool)".
;; Un procedimiento sin argumentos se escribe "(-> num)".
(define tipo->texto
  (lambda (t)
    (cases tipo t
      (num-tipo () "num")
      (bool-tipo () "bool")
      (ref-tipo (interno)
                (string-append "refto(" (tipo->texto interno) ")"))
      (proc-tipo (args res)
                 (string-append "("
                                (unir-textos (map tipo->texto args) " * ")
                                (if (null? args) "-> " " -> ")
                                (tipo->texto res)
                                ")")))))

;; listas-de-tipos-iguales? : lista de tipo x lista de tipo -> boolean
;; Dos listas de tipos son iguales si tienen la misma longitud y los
;; tipos de cada posición son iguales.
(define listas-de-tipos-iguales?
  (lambda (ts1 ts2)
    (cond ((and (null? ts1) (null? ts2)) #t)
          ((or (null? ts1) (null? ts2)) #f)
          (else (and (tipos-iguales? (car ts1) (car ts2))
                     (listas-de-tipos-iguales? (cdr ts1) (cdr ts2)))))))

;; tipos-iguales? : tipo x tipo -> boolean
;; Compara dos tipos por estructura, no por identidad.
(define tipos-iguales?
  (lambda (t1 t2)
    (cases tipo t1
      (num-tipo ()
                (cases tipo t2
                  (num-tipo () #t)
                  (else #f)))
      (bool-tipo ()
                 (cases tipo t2
                   (bool-tipo () #t)
                   (else #f)))
      (ref-tipo (interno1)
                (cases tipo t2
                  (ref-tipo (interno2) (tipos-iguales? interno1 interno2))
                  (else #f)))
      (proc-tipo (args1 res1)
                 (cases tipo t2
                   (proc-tipo (args2 res2)
                              (and (listas-de-tipos-iguales? args1 args2)
                                   (tipos-iguales? res1 res2)))
                   (else #f))))))

;; tenv : TAD
;; El ambiente de tipos Γ, en paralelo con el ambiente de valores:
;;   tenv-vacio       el ambiente sin declaraciones
;;   tenv-extendido   ids declarados con los tipos tipos sobre el ambiente amb
;;   tenv-recursivo   los procedimientos de un letrec con su tipo declarado
;;                    (un proc-tipo por cada nombre), sobre el ambiente amb
(define-datatype tenv tenv?
  (tenv-vacio)
  (tenv-extendido
   (ids (list-of symbol?))
   (tipos (list-of tipo?))
   (amb tenv?))
  (tenv-recursivo
   (names (list-of symbol?))
   (tipos (list-of tipo?))
   (amb tenv?)))

;; posicion-en : symbol x lista de symbol -> integer o #f
;; La posición del símbolo en la lista, o #f si no está.
(define posicion-en
  (lambda (id ids)
    (letrec ((buscar (lambda (lst i)
                       (cond ((null? lst) #f)
                             ((eqv? (car lst) id) i)
                             (else (buscar (cdr lst) (+ i 1)))))))
      (buscar ids 0))))

;; apply-tenv : tenv x symbol -> tipo
;; Busca el tipo del identificador. Si no está declarado levanta
;; eopl:error con «Variable no declarada: <x>».
(define apply-tenv
  (lambda (amb id)
    (cases tenv amb
      (tenv-vacio ()
                  (eopl:error 'apply-tenv "Variable no declarada: ~s" id))
      (tenv-extendido (ids tipos saved-amb)
                      (let ((pos (posicion-en id ids)))
                        (if pos
                            (list-ref tipos pos)
                            (apply-tenv saved-amb id))))
      (tenv-recursivo (names tipos saved-amb)
                      (let ((pos (posicion-en id names)))
                        (if pos
                            (list-ref tipos pos)
                            (apply-tenv saved-amb id)))))))

;; tenv-inicial : -> tenv
;; Declara como num las cinco variables del ambiente inicial.
(define tenv-inicial
  (lambda ()
    (tenv-extendido '(n m p q r)
                    (list (num-tipo) (num-tipo) (num-tipo) (num-tipo) (num-tipo))
                    (tenv-vacio))))

;; nombre-de-primitiva : primitiva -> symbol
;; El símbolo con que la primitiva se escribe en el programa.
(define nombre-de-primitiva
  (lambda (prim)
    (cases primitiva prim
      (sum-prim () '+)
      (minus-prim () '-)
      (mult-prim () '*)
      (div-prim () '/)
      (add1-prim () 'add1)
      (sub1-prim () 'sub1)
      (less-prim () '<)
      (greater-prim () '>)
      (less-eq-prim () '<=)
      (greater-eq-prim () '>=)
      (equal-prim () '==)
      (not-equal-prim () '!=)
      (and-prim () 'and)
      (or-prim () 'or)
      (not-prim () 'not)
      (entre-prim () 'entre))))

;; verificar-cantidad : symbol x lista de tipo x integer -> boolean
;; Retorna #t si la lista tiene n tipos; si no, levanta eopl:error con la
;; cantidad esperada y la recibida.
(define verificar-cantidad
  (lambda (nombre tipos n)
    (if (= (length tipos) n)
        #t
        (eopl:error nombre "La primitiva espera ~s argumentos y recibio ~s"
                    n (length tipos)))))

;; verificar-argumentos : symbol x tipo x lista de tipo x integer -> boolean
;; Retorna #t si todos los tipos de la lista son iguales al esperado; si
;; no, levanta eopl:error indicando el número del argumento. El último
;; parámetro es el número del primer argumento de la lista.
(define verificar-argumentos
  (lambda (nombre esperado tipos i)
    (cond ((null? tipos) #t)
          ((tipos-iguales? esperado (car tipos))
           (verificar-argumentos nombre esperado (cdr tipos) (+ i 1)))
          (else
           (eopl:error nombre "El argumento ~s debe ser ~a, se encontro ~a"
                       i (tipo->texto esperado) (tipo->texto (car tipos)))))))

;; exigir-y-retornar : symbol x tipo x integer x tipo x lista de tipo -> tipo
;; Verifica que haya n argumentos y que todos sean del tipo esperado, y
;; retorna el tipo del resultado.
(define exigir-y-retornar
  (lambda (nombre esperado n resultado tipos)
    (verificar-cantidad nombre tipos n)
    (verificar-argumentos nombre esperado tipos 1)
    resultado))

;; tipo-de-primitiva : primitiva x lista de tipos -> tipo
;; Lo que cada primitiva exige de sus argumentos y lo que retorna: las
;; aritméticas retornan num; las relacionales, las booleanas y entre
;; retornan bool. La cantidad de argumentos es la misma que exige el
;; evaluador, para que el chequeador no acepte lo que el evaluador rechaza.
(define tipo-de-primitiva
  (lambda (prim tipos)
    (let ((nombre (nombre-de-primitiva prim)))
      (cases primitiva prim
        (sum-prim () (exigir-y-retornar nombre (num-tipo) 2 (num-tipo) tipos))
        (minus-prim () (exigir-y-retornar nombre (num-tipo) 2 (num-tipo) tipos))
        (mult-prim () (exigir-y-retornar nombre (num-tipo) 2 (num-tipo) tipos))
        (div-prim () (exigir-y-retornar nombre (num-tipo) 2 (num-tipo) tipos))
        (add1-prim () (exigir-y-retornar nombre (num-tipo) 1 (num-tipo) tipos))
        (sub1-prim () (exigir-y-retornar nombre (num-tipo) 1 (num-tipo) tipos))
        (less-prim () (exigir-y-retornar nombre (num-tipo) 2 (bool-tipo) tipos))
        (greater-prim () (exigir-y-retornar nombre (num-tipo) 2 (bool-tipo) tipos))
        (less-eq-prim () (exigir-y-retornar nombre (num-tipo) 2 (bool-tipo) tipos))
        (greater-eq-prim () (exigir-y-retornar nombre (num-tipo) 2 (bool-tipo) tipos))
        (equal-prim () (exigir-y-retornar nombre (num-tipo) 2 (bool-tipo) tipos))
        (not-equal-prim () (exigir-y-retornar nombre (num-tipo) 2 (bool-tipo) tipos))
        (and-prim () (exigir-y-retornar nombre (bool-tipo) 2 (bool-tipo) tipos))
        (or-prim () (exigir-y-retornar nombre (bool-tipo) 2 (bool-tipo) tipos))
        (not-prim () (exigir-y-retornar nombre (bool-tipo) 1 (bool-tipo) tipos))
        (entre-prim () (exigir-y-retornar nombre (num-tipo) 3 (bool-tipo) tipos))))))

(provide (all-defined-out))
