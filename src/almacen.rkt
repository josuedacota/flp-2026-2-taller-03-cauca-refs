#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

#|
  Taller 3 — Cauca con referencias y tipos.
  TAD referencia y almacén (Parte 1).

  El módulo no requiere ningún otro: el almacén guarda valores
  cualesquiera y no necesita la gramática.

  Falta el datatype de la referencia, que es un tipo abstracto y no un
  entero desnudo:

    (define-datatype referencia referencia?
      (a-ref (pos integer?)))

  Falta también la variable global con el almacén. Es el único lugar
  del curso donde se usa set!.

  Invariante que las operaciones tienen que preservar, y que se demuestra
  en la Parte 5:
    1. toda referencia entregada por newref! apunta a una celda existente
    2. el almacén solo crece por newref!
    3. setref! cambia una celda y deja las demás intactas
    4. deref sobre una referencia fuera de rango levanta eopl:error
|#

;; referencia : TAD
;; a-ref guarda la posición de la celda dentro del almacén.
(define-datatype referencia referencia?
  (a-ref (pos integer?)))

;; the-store : almacen
;; El almacén global: la lista de celdas, indexadas desde 0.
(define the-store '())

;; empty-store : -> almacen
;; El almacén sin celdas.
(define empty-store
  (lambda ()
    '()))

;; initialize-store! : -> void
;; Deja el almacén vacío. Se llama al empezar cada programa.
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;; posicion-valida? : integer -> boolean
;; Dice si la posición nombra una celda existente del almacén actual.
(define posicion-valida?
  (lambda (pos)
    (and (>= pos 0) (< pos (length the-store)))))

;; newref! : valor -> referencia
;; Agrega una celda con el valor y retorna la referencia que la nombra.
(define newref!
  (lambda (val)
    (let ((pos (length the-store)))
      (set! the-store (append the-store (list val)))
      (a-ref pos))))

;; deref : referencia -> valor
;; Lee la celda. Si la referencia está fuera de rango, levanta eopl:error.
(define deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos)
             (if (posicion-valida? pos)
                 (list-ref the-store pos)
                 (eopl:error 'deref "Referencia fuera de rango: ~s" pos))))))

;; reemplazar-en : lista x integer x valor -> lista
;; Copia de la lista con el elemento de la posición cambiado por el valor.
;; No toca ninguna otra posición.
(define reemplazar-en
  (lambda (lst pos val)
    (if (= pos 0)
        (cons val (cdr lst))
        (cons (car lst) (reemplazar-en (cdr lst) (- pos 1) val)))))

;; setref! : referencia x valor -> void
;; Escribe el valor en la celda y deja las demás intactas.
(define setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos)
             (if (posicion-valida? pos)
                 (set! the-store (reemplazar-en the-store pos val))
                 (eopl:error 'setref! "Referencia fuera de rango: ~s" pos))))))

(provide (all-defined-out))
