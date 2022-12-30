#!/usr/bin/env -S guile -s
!#
(use-modules
 (toml builder)
 (json)
 (srfi srfi-1)
 (ice-9 textual-ports)
 (ice-9 pretty-print))

(define test-value?
  (lambda (expr)
    (or
     (vector? expr)
     (and
      (list? expr)
      (or (equal? (map car expr) '("type" "value"))
          (equal? (map car expr) '("value" "type")))))))

(define (build-atomic-value scm port)
  (define (find-cdr-by-car s lst)
    (cdr (find (lambda (x) (equal? (car x) s)) lst)))
  (define type (find-cdr-by-car "type" scm))
  (define value (find-cdr-by-car "value" scm))
  (put-string port value))

(define* (test-toml-build-value scm port #:key (newline? #t) (inline? #t))
  (cond
   ((vector? scm)
    (toml-build-array scm port))
   (else
    (build-atomic-value scm port))))

(set-port-conversion-strategy! (current-input-port) 'error)

(define scm (json->scm (current-input-port)))

(pretty-print scm)
;; (test-value? '())

;;(value->scm test-value->scm)
(define x (parameterize ((toml-build-value test-toml-build-value)
                         (value? test-value?))
            (scm->toml scm)))
;; (scm->toml scm)
