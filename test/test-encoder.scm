#!/usr/bin/env -S guile -s
!#
(use-modules
 (toml builder)
 (json))

(define test-value?
  (lambda (expr)
    (and
     (string? (car expr)))
    (or
     (vector? (cdr expr))
     (equal? (map car (cdr expr)) '("value" "type")))))

(set-port-conversion-strategy! (current-input-port) 'error)

(define scm (json->scm (current-input-port)))


;;(value->scm test-value->scm)
(define x (parameterize ((value? test-value?))
            (scm->toml scm)))
;; (scm->toml scm)
