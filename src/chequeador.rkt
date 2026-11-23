#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

#|
  Taller 3 - Cauca con referencias y tipos.
  Reglas de tipado y chequeo del programa (Parte 3).

  type-of recorre el arbol con cases sobre un ambiente de tipos y no
  evalua nada: una regla por variante de expresion, tal como estan
  escritas en el cuadro del enunciado.

  Los mensajes de error se distinguen uno de otro.

  El modulo requiere sintaxis.rkt y tipos.rkt, y ningun otro modulo del
  interprete.
|#

(require "sintaxis.rkt")
(require "tipos.rkt")

;; type-of-lista : lista de expresion x tenv -> lista de tipo
;; Calcula el tipo de cada expresion de la lista, de izquierda a derecha.
(define type-of-lista
  (lambda (exps amb)
    (if (null? exps)
        '()
        (let ((primero (type-of (car exps) amb)))
          (cons primero (type-of-lista (cdr exps) amb))))))

;; exigir-tipo : tipo x tipo x string -> tipo
;; Retorna el tipo recibido si es igual al esperado; si no, levanta eopl:error con el mensaje.
(define exigir-tipo
  (lambda (esperado encontrado formato)
    (if (tipos-iguales? esperado encontrado)
        encontrado
        (eopl:error 'type-of formato (tipo->texto encontrado)))))

;; verificar-argumentos-app : lista de tipo x lista de tipo x integer -> boolean
;; Verifica que cada tipo de argumento sea el del parametro; i es el numero del primero.
(define verificar-argumentos-app
  (lambda (esperados encontrados i)
    (cond ((null? esperados) #t)
          ((tipos-iguales? (car esperados) (car encontrados))
           (verificar-argumentos-app (cdr esperados) (cdr encontrados) (+ i 1)))
          (else
           (eopl:error 'type-of "El argumento ~s debe ser ~a, se encontro ~a"
                       i (tipo->texto (car esperados)) (tipo->texto (car encontrados)))))))

;; verificar-aridad-app : lista de tipo x lista -> boolean
;; Verifica que la cantidad de parametros coincida con la de argumentos.
(define verificar-aridad-app
  (lambda (esperados recibidos)
    (if (= (length esperados) (length recibidos))
        #t
        (eopl:error 'type-of "El procedimiento espera ~s argumentos y recibio ~s"
                    (length esperados) (length recibidos)))))

;; exigir-procedimiento : tipo -> tipo
;; Retorna el tipo si es de procedimiento; si no, levanta eopl:error.
(define exigir-procedimiento
  (lambda (t)
    (cases tipo t
      (proc-tipo (args res) t)
      (else (eopl:error 'type-of "Se intento aplicar un valor de tipo ~a" (tipo->texto t))))))

;; tipo-de-argumentos-ref : lista de symbol x tenv -> lista de tipo
;; Busca el tipo de cada identificador de applyref y exige que sea refto(s).
(define tipo-de-argumentos-ref
  (lambda (ids amb)
    (if (null? ids)
        '()
        (let ((t (apply-tenv amb (car ids))))
          (cases tipo t
            (ref-tipo (interno)
                      (cons t (tipo-de-argumentos-ref (cdr ids) amb)))
            (else (eopl:error 'type-of
                              "applyref exige variables de tipo refto(t), ~s tiene tipo ~a"
                              (car ids) (tipo->texto t))))))))

;; tipo-de-ref : tipo x symbol -> tipo
;; Retorna el tipo contenido si t es refto(t'); si no, levanta eopl:error.
(define tipo-de-ref
  (lambda (t quien)
    (cases tipo t
      (ref-tipo (interno) interno)
      (else (eopl:error 'type-of "~a espera refto(t), se encontro ~a" quien (tipo->texto t))))))

;; tipos-de-procedimientos : lista de tipo x lista de lista de tipo -> lista de tipo
;; Arma el proc-tipo declarado de cada procedimiento de un letrec.
(define tipos-de-procedimientos
  (lambda (tipos-res tipos-args)
    (if (null? tipos-res)
        '()
        (cons (proc-tipo (car tipos-args) (car tipos-res))
              (tipos-de-procedimientos (cdr tipos-res) (cdr tipos-args))))))

;; verificar-cuerpos-letrec : lista de symbol x lista de lista de symbol x lista de lista de tipo
;;                            x lista de tipo x lista de expresion x tenv -> boolean
;; Chequea cada cuerpo de letrec en el ambiente recursivo extendido con sus parametros.
(define verificar-cuerpos-letrec
  (lambda (names idss tipos-args tipos-res bodies amb)
    (if (null? names)
        #t
        (let ((t-cuerpo (type-of (car bodies)
                                 (tenv-extendido (car idss) (car tipos-args) amb))))
          (if (tipos-iguales? (car tipos-res) t-cuerpo)
              (verificar-cuerpos-letrec (cdr names) (cdr idss) (cdr tipos-args)
                                        (cdr tipos-res) (cdr bodies) amb)
              (eopl:error 'type-of "El procedimiento ~a declara ~a y su cuerpo da ~a"
                          (car names) (tipo->texto (car tipos-res)) (tipo->texto t-cuerpo)))))))

;; verificar-ramas-case : tipo x lista de expresion x tenv -> boolean
;; Verifica que todas las ramas del case tengan el tipo t.
(define verificar-ramas-case
  (lambda (t exps amb)
    (if (null? exps)
        #t
        (let ((t-rama (type-of (car exps) amb)))
          (if (tipos-iguales? t t-rama)
              (verificar-ramas-case t (cdr exps) amb)
              (eopl:error 'type-of "Las ramas del case deben coincidir: ~a vs ~a"
                          (tipo->texto t) (tipo->texto t-rama)))))))

;; ultimo-tipo : lista de tipo -> tipo
;; El ultimo tipo de una lista no vacia.
(define ultimo-tipo
  (lambda (tipos)
    (if (null? (cdr tipos))
        (car tipos)
        (ultimo-tipo (cdr tipos)))))

;; type-of : expresion x tenv -> tipo
;; El tipo de la expresion bajo el ambiente de tipos, o eopl:error con
;; el mensaje que explique el conflicto.
(define type-of
  (lambda (exp amb)
    (cases expresion exp
      (lit-exp (n) (num-tipo))
      (var-exp (id) (apply-tenv amb id))
      (true-exp () (bool-tipo))
      (false-exp () (bool-tipo))
      (prim-exp (prim rands)
                (tipo-de-primitiva prim (type-of-lista rands amb)))
      (if-exp (test-exp true-exp false-exp)
              (let ((t-test (type-of test-exp amb)))
                (exigir-tipo (bool-tipo) t-test "La condicion del if debe ser bool, se encontro ~a")
                (let ((t1 (type-of true-exp amb))
                      (t2 (type-of false-exp amb)))
                  (if (tipos-iguales? t1 t2)
                      t1
                      (eopl:error 'type-of "Las ramas del if deben coincidir: ~a vs ~a"
                                  (tipo->texto t1) (tipo->texto t2))))))
      (let-exp (ids rands body)
               (type-of body (tenv-extendido ids (type-of-lista rands amb) amb)))
      (proc-exp (ids tipos body)
                (proc-tipo tipos (type-of body (tenv-extendido ids tipos amb))))
      (app-exp (rator rands)
               (cases tipo (exigir-procedimiento (type-of rator amb))
                 (proc-tipo (args res)
                            (verificar-aridad-app args rands)
                            (verificar-argumentos-app args (type-of-lista rands amb) 1)
                            res)
                 (else (eopl:error 'type-of "Se intento aplicar algo que no es un procedimiento"))))
      (app-ref-exp (rator ids)
                   (cases tipo (exigir-procedimiento (type-of rator amb))
                     (proc-tipo (args res)
                                (verificar-aridad-app args ids)
                                (verificar-argumentos-app args (tipo-de-argumentos-ref ids amb) 1)
                                res)
                     (else (eopl:error 'type-of "Se intento aplicar algo que no es un procedimiento"))))
      (letrec-exp (tipos-res proc-names idss tipos-args bodies letrec-body)
                  (let ((amb-rec (tenv-recursivo proc-names
                                                 (tipos-de-procedimientos tipos-res tipos-args)
                                                 amb)))
                    (verificar-cuerpos-letrec proc-names idss tipos-args tipos-res bodies amb-rec)
                    (type-of letrec-body amb-rec)))
      (case-exp (e nums exps default)
                (begin
                  (exigir-tipo (num-tipo) (type-of e amb)
                               "La expresion del case debe ser num, se encontro ~a")
                  (let ((t (type-of default amb)))
                    (verificar-ramas-case t exps amb)
                    t)))
      (newref-exp (e)
                  (ref-tipo (type-of e amb)))
      (deref-exp (e)
                 (tipo-de-ref (type-of e amb) 'deref))
      (setref-exp (ref-exp val-exp)
                  (let* ((t-celda (tipo-de-ref (type-of ref-exp amb) 'setref))
                         (t-valor (type-of val-exp amb)))
                    (if (tipos-iguales? t-celda t-valor)
                        (num-tipo)
                        (eopl:error 'type-of "La celda guarda ~a y se intento escribir ~a"
                                    (tipo->texto t-celda) (tipo->texto t-valor)))))
      (swap-exp (ref1-exp ref2-exp)
                (let* ((t1 (tipo-de-ref (type-of ref1-exp amb) 'swap))
                       (t2 (tipo-de-ref (type-of ref2-exp amb) 'swap)))
                  (if (tipos-iguales? t1 t2)
                      t1
                      (eopl:error 'type-of "swap exige celdas del mismo tipo: refto(~a) vs refto(~a)"
                                  (tipo->texto t1) (tipo->texto t2)))))
      (begin-exp (first-exp rest-exps)
                 (ultimo-tipo (type-of-lista (cons first-exp rest-exps) amb))))))

;; type-of-program : programa -> tipo
;; Chequea el programa en el ambiente de tipos inicial.
(define type-of-program
  (lambda (pgm)
    (cases programa pgm
      (a-program (body) (type-of body (tenv-inicial))))))

;; tipo-de-texto : string -> string
;; Analiza el texto, lo chequea y describe el tipo resultante.
(define tipo-de-texto
  (lambda (texto)
    (tipo->texto (type-of-program (parser texto)))))

(provide (all-defined-out))