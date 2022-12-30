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
      (('datetime v)
       (validate-date-time `(datetime ,v))
       `(("value" . ,v) ("type" . "datetime")))
      (('datetime-local v)
       (validate-date-time `(datetime-local ,v))
       `(("value" . ,v) ("type" . "datetime-local")))
      (('date-local v)
       (validate-date-time `(date-local ,v))
       `(("value" . ,v) ("type" . "date-local")))
      (('time-local v)
       (validate-date-time `(time-local ,v))
       `(("value" . ,v) ("type" . "time-local")))
      (('inline-table "")
       '())
      (('inline-table xs ...)
       (peg-tree->scm (flatten-tree xs)))
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


(set-port-conversion-strategy! (current-input-port) 'error)

(define str (get-string-all (current-input-port)))

(define scm (parameterize ((value->scm test-value->scm)
                           (value? test-value?))
              (toml->scm str)))

(define json (scm->json scm #:pretty #t #:unicode #t))
