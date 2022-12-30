(define-module (toml)
  #:use-module (toml parser)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (toml->scm))


(define-syntax-rule (log-exprs exp ...) (begin (format #t "~a: ~S\n" (quote exp) exp) ...))

(define (flatten-array l)
  (keyword-flatten '(array dec-int float string bola) l))

(define (get-keys l)
  (map cadr (keyword-flatten '(simple-key) l)))

(define (single-value-proc x y)
  y)

(define (annot-v-proc x y)
  `(("value" . ,y) ("type" . ,(symbol->string x))))


(define (value->scm v)
  (match v
    (('array vs ...)
     ;; (pretty-print (flatten-array vs))
     (list->vector (map value->scm (flatten-array vs))))
    ;; (format #f "array ~a" (flatten-array vs)))
    ((x y)
     ;; (single-value-proc x y)
     (annot-v-proc x y))
    ;; (format #f "type: ~a, value: ~a" x y))
    ('()
     '())
    ;; ('inline-table
    ;;  '())
    (_ (error "err: ~a" v))))

(define (keyval->scm keys value)
  (let loop ((keys keys))
    (if (null? (cdr keys))
        (cons (car keys) (value->scm (car value)))
        (list (car keys) (loop (cdr keys))))))

;; (define tr '(("a" ("aa" . "v")) ("b" "c")))
(define (value? expr)
  (list? expr))

(define (test-value? expr)
  (and
   (string? (car expr))
   (equal? (map car (cdr expr)) '("value" "type"))))
;; (list-index (lambda (x) (equal? x "c")) (map car tr))

(define (add-to-tree tree keys value)
  ;; (pretty-print value)
  (if (null? keys)
      ;; TODO helper to never call this on top-level
      (value->scm value)
      (let ((k (list-index (lambda (x) (equal? x (car keys))) (map car tree))))
        (if k
            (let ((e (list-ref tree k)))
              ;; (pretty-print e)
              (when (test-value? e)
                (error "guile-toml: redefinition not allowed"))
              (append
               (take tree k)
               (list (cons
                      (car e)
                      (add-to-tree (cdr e) (cdr keys) value)))
               (drop tree (1+ k))))
            (cons (keyval->scm keys value) tree)))))

(define (heads lst)
  (map (lambda (k) (list-head lst k)) (iota (length lst) 1)))

(define (check-inline-table-keys keylist inline-table-keys)
  (let ((heads (map (lambda (k) (list-head keylist k))
                    (iota (length keylist) 1))))
    (when (any (lambda (x) (member x inline-table-keys))
               heads)
      (error "guile-toml: redefinition not allowed"))))

(define (toml->scm s)
  (define tree (parse s))
  (let loop ((tree (if (symbol? (car tree)) (list tree) tree))
             (result '())
             (current-table '())
             (inline-table-keys '()))
    ;; (pretty-print tree)
    (match (car tree)
      (('keyval keys 'inline-table)
       (let ((keylist (append current-table (get-keys keys))))
         (set! inline-table-keys (cons keylist inline-table-keys))
         (set! result (add-to-tree result keylist '(())))))
      (('keyval keys ('inline-table keyvals ...))
       (set! result
             (loop (keyword-flatten '(keyval) keyvals)
                   result
                   (get-keys keys)
                   '())))
      (('keyval keys value ...)
       (let ((keylist (append current-table (get-keys keys))))
         (check-inline-table-keys keylist inline-table-keys)
         (set! result (add-to-tree result keylist value))))
      (('std-table keys ...)
       (let ((keylist (get-keys keys)))
         (check-inline-table-keys keylist inline-table-keys)
         (set! result (add-to-tree result keylist '(())))
         (set! current-table keylist)))

      (x (format #t "~a ; unknown: ~a\n" tree x)))

    (if (null? (cdr tree))
        result
        (loop (cdr tree) result current-table inline-table-keys))))


;; (toml->scm "a={}\n[a]")
;; (parse "a=2")
;; (parse "[a]")
