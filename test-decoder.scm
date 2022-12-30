#!/usr/bin/env -S guile -s
!#
(use-modules (json) (toml) (ice-9 match) (ice-9 textual-ports) (ice-9 pretty-print))

(define test-value->scm
  (lambda (v)
    (match v
      (('array vs ...)
       (list->vector (map test-value->scm (flatten-array vs))))
      (('string ys ...)
       `(("value" . ,(read-string ys)) ("type" . "string")))
      ('string
       `(("value" . "") ("type" . "string")))
      (('integer x)
       `(("value" . ,(number->string (read-int x))) ("type" . "integer")))
      ((x y)
       `(("value" . ,y) ("type" . ,(symbol->string x))))
      ('()
       '())
      (_ (error "err: ~a" v)))))


(define test-value?
  (lambda (expr)
    (and
     (string? (car expr))
     (or
      (vector? (cdr expr))
      (equal? (map car (cdr expr)) '("value" "type"))))))

(define str (get-string-all (current-input-port)))

(define scm (parameterize ((value->scm test-value->scm)
                           (value? test-value?))
              (toml->scm str)))
;; (pretty-print scm)

(define json (scm->json scm #:pretty #t #:unicode #t))
