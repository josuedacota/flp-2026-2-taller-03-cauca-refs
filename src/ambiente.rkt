#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

#|
  Taller 3 — Cauca con referencias y tipos.
  TAD ambiente y valor procedimiento.

  Las dos definiciones van en el mismo módulo porque se nombran entre
  sí: el campo del ambiente dentro de una clausura pide ambiente?, y
  apply-env construye una clausura cuando resuelve un nombre ligado por
  letrec. Separarlas deja uno de los dos campos sin predicado.

  Faltan los dos datatypes:
    - ambiente, con las variantes ambiente-vacio, ambiente-extendido y
      ambiente-recursivo
    - procval, con la variante de la clausura: los parámetros, el cuerpo
      y el ambiente donde se creó

  El cuerpo de una clausura es una expresion?, que viene de sintaxis.rkt.
|#

(require "sintaxis.rkt")

;; cualquier-valor? : any -> boolean
;; Predicado del campo de valores del ambiente: acepta cualquier valor
;; (ver la nota de dependencias en el encabezado del módulo).
(define cualquier-valor?
  (lambda (v) #t))

;; ambiente : TAD
;; El ambiente de valores, con las tres variantes de EOPL:
;;   ambiente-vacio       el ambiente sin ligaduras
;;   ambiente-extendido   ids ligados a vals sobre el ambiente env
;;   ambiente-recursivo   los procedimientos de un letrec: nombres,
;;                        parámetros y cuerpos, sobre el ambiente env
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (ids (list-of symbol?))
   (vals (list-of cualquier-valor?))
   (env ambiente?))
  (ambiente-recursivo
   (names (list-of symbol?))
   (idss (list-of (list-of symbol?)))
   (bodies (list-of expresion?))
   (env ambiente?)))

;; procval : TAD
;; El valor procedimiento: la clausura con los parámetros, el cuerpo y el
;; ambiente donde se creó.
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env ambiente?)))

;; posicion-de : symbol x lista de symbol -> integer o #f
;; La posición del símbolo en la lista, o #f si no está. Si el símbolo
;; aparece más de una vez, la primera posición.
(define posicion-de
  (lambda (id ids)
    (letrec ((buscar (lambda (lst i)
                       (cond ((null? lst) #f)
                             ((eqv? (car lst) id) i)
                             (else (buscar (cdr lst) (+ i 1)))))))
      (buscar ids 0))))

;; apply-env : ambiente x symbol -> valor
;; Busca la ligadura del identificador. Sobre el ambiente vacío levanta
;; eopl:error; sobre el recursivo construye la clausura del procedimiento.
(define apply-env
  (lambda (env id)
    (cases ambiente env
      (ambiente-vacio ()
                      (eopl:error 'apply-env "Variable no ligada: ~s" id))
      (ambiente-extendido (ids vals saved-env)
                          (let ((pos (posicion-de id ids)))
                            (if pos
                                (list-ref vals pos)
                                (apply-env saved-env id))))
      (ambiente-recursivo (names idss bodies saved-env)
                          (let ((pos (posicion-de id names)))
                            (if pos
                                (closure (list-ref idss pos) (list-ref bodies pos) env)
                                (apply-env saved-env id)))))))

;; ambiente-inicial : -> ambiente
;; El del enunciado: n = 10, m = 3, p = 7, q = 2, r = 5.
(define ambiente-inicial
  (lambda ()
    (ambiente-extendido '(n m p q r)
                        '(10 3 7 2 5)
                        (ambiente-vacio))))

(provide (all-defined-out))
