#!/usr/bin/env -S guile -s
!#
(use-modules (json) (toml) (ice-9 match) (ice-9 textual-ports))

(define test-value->scm
  (lambda (v)
    (match v
      (('array vs ...)
       ;; (pretty-print (flatten-array vs))
       (list->vector (map test-value->scm (flatten-array vs))))
      ;; (format #f "array ~a" (flatten-array vs)))
      ((x y)
       ;; (single-value-proc x y)
       `(("value" . ,y) ("type" . ,(symbol->string x))))
      ;; (format #f "type: ~a, value: ~a" x y))
      ('()
       '())
      ;; ('inline-table
      ;;  '())
      (_ (error "err: ~a" v)))))


(define test-value?
  (lambda (expr)
    (and
     (string? (car expr))
     (equal? (map car (cdr expr)) '("value" "type")))))

(define str (get-string-all (current-input-port)))

(define scm (parameterize ((value->scm test-value->scm)
                           (value? test-value?))
              (toml->scm str)))

(define json (scm->json scm #:pretty #t))
