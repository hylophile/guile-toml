(define-module (toml)
  #:use-module (toml parser)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  ;; TODO exporting flatten-array isn't nice, it's an internal function.
  #:export (toml->scm flatten-array value->scm read-string read-int value?))


(define-syntax-rule (log-exprs exp ...) (begin (format #t "~a: ~S\n" (quote exp) exp) ...))

(define (flatten-array l)
  (keyword-flatten '(string bool array inline-table date-time float integer) l))

(define (get-keys l)
  (map (lambda (k) (read-string (cdr k))) (keyword-flatten '(simple-key) l)))

(define (unicode-point->string s)
  (string (integer->char (string->number (substring s 2) 16))))

(define (unescape-escaped s)
  (if (list? s)
      (match (string-ref (cadr s) 1)
        (#\\ "\\")
        (#\" "\"")
        (#\b "\b")
        (#\f "\f")
        (#\n "\n")
        (#\r "\r")
        (#\t "\t")
        (#\u (unicode-point->string (cadr s)))
        (#\U (unicode-point->string (cadr s)))
        (_ (error "guile-toml: unsupported escape char:" (cadr s))))
      s))

(define (read-string lst)
  (string-join (map unescape-escaped (keyword-flatten '(escaped) lst)) ""))

(define (read-int str)
  (define base (false-if-exception (substring str 0 2)))
  (define data (false-if-exception (substring str 2)))
  (match base
    ("0b" (string->number data 2))
    ("0o" (string->number data 8))
    ("0x" (string->number data 16))
    (_ (string->number str 10))))

(define v "-.2e28")
;; we want to be able to dynamically bind this function in test-decoder.scm
(define value->scm
  (make-parameter
   (lambda (value-pair)
     (match value-pair
       (('array value-pairs ...)
        (list->vector (map (value->scm) (flatten-array value-pairs))))
       (('integer v)
        (read-int v))
       (('float v)
        ;; could make this nicer by tagging nan/inf already in PEGs
        (cond
         ((string-contains v "nan") #i+nan.0)
         ((string-contains v "-inf") #i-inf.0)
         ((string-contains v "inf") #i+inf.0)
         (else (string->number v))))
       (('string vs ...)
        (read-string vs))
       ('string
        "")
       (('bool v)
        (equal? v "true"))
       (('datetime v)
        (display "guile-toml: datetimes are currently not supported\n")
        v)
       (('datetime-local v)
        (display "guile-toml: datetimes are currently not supported\n")
        v)
       (('date-local v)
        (display "guile-toml: datetimes are currently not supported\n")
        v)
       (('time-local v)
        (display "guile-toml: datetimes are currently not supported\n")
        v)

       ((x y)
        (format #f "~a: ~a" x y))
       ('()
        '())
       ('(())
        (error "redef"))
       ;; ('inline-table
       ;;  '())
       (_ (error "err:" value-pair))))))

;; ((value->scm) '(x "2"))

(define (keyval->scm keys value array-table-index)
  (let loop ((keys keys))
    (cond
     ;; ((and array-table-index (eq? 2 (length keys)))
     ;;  (cons (first keys) (list->vector (list (list (cons (second keys) ((value->scm) (car value))))))))

     ((null? (cdr keys))
      (cons (car keys) ((value->scm) (car value))))
     (else
      (list (car keys) (loop (cdr keys)))))))

;; we want to be able to dynamically bind this functin in test-decoder.scm
(define value?
  (make-parameter
   (lambda (expr) (not (list? expr)))))

(define (find-subtree tree keys)
  (cond ((null? keys) #f)
        (else (let ((k (list-index (lambda (x) (equal? x (car keys)))
                                   (map car tree))))
                (and k (list-ref tree k))))))


(define (add-to-tree tree table-keys keys value array-table-index)
  (if array-table-index
      (add-kv-to-tree-in-array tree table-keys keys value array-table-index)
      (add-kv-to-tree tree (append table-keys keys) value)))

(define (init-array-table keys value)
  (cons (first keys) (list->vector `((,(cons (second keys) value))))))

;; (define (keyval->scm keys value array-table-index))
(define t (init-array-table '("a" "b") "3"))

;; (vector-ref #((1) 2) 0)

(define (add-kv-to-array array keys value index)
  (define l (vector->list array))
  (list-set! l
             index
             (add-kv-to-tree (vector-ref array index) keys value))
  (list->vector l))


;; (add-kv-to-array #((("a" . 2) ("b" . 2)) (("d" . 2))) '("c") '((integer "3")) 0)

;; (vector-ref (vector->list #((()))) 0)
;; (define (add-to-array-table tree table-keys keys value index)
;;   (define table-content ())
;;   3)

(define (add-kv-to-tree-in-array tree table-keys keys value index)
  ;; (pretty-print value)
  (if (null? (cdr table-keys))
      (begin
        ;; (log-exprs "null" tree keys value (and (pair? tree) (cdr tree)))
        (if (null? tree)
            (list (cons (car table-keys) (add-kv-to-array (if (null? tree) #(()) (cdr tree)) keys value index)))
            (let ((k (list-index (lambda (x) (equal? x (car table-keys))) (map car tree))))
              ;; (log-exprs tree keys value k)
              (if k
                  (let ((e (list-ref tree k)))
                    (append
                     (take tree k)
                     (list (cons
                            (car e)
                            (add-kv-to-array (cdr e) keys value index)))
                     (drop tree (1+ k))))
                  (cons (cons (car table-keys) (add-kv-to-array #(()) keys value index)) tree)))))))

(define (add-kv-to-tree tree keys value)
  ;; (pretty-print value)
  (let ((k (list-index (lambda (x) (equal? x (car keys))) (map car tree))))
    (if k
        (let ((e (list-ref tree k)))
          (cond
           (((value?) e)
            (error "guile-toml: redefinition not allowed"))
           (else
            (append
             (take tree k)
             (list (cons
                    (car e)
                    (add-kv-to-tree (cdr e) (cdr keys) value)))
             (drop tree (1+ k))))))
        (cons (keyval->scm keys value #f) tree))))

(define (heads lst)
  (map (lambda (k) (list-head lst k)) (iota (length lst) 1)))

(define (check-inline-table-keys keylist inline-table-keys)
  (let ((heads (map (lambda (k) (list-head keylist k))
                    (iota (length keylist) 1))))
    (when (any (lambda (x) (member x inline-table-keys))
               heads)
      (error "guile-toml: redefinition not allowed"))))

(define (find-key tree keys)
  (let ((k (list-index (lambda (x) (equal? x (car keys))) (map car tree))))
    (cond
     ((and k (null? (cdr keys))) (list-ref tree k))
     (k (find-key (cdr (list-ref tree k)) (cdr keys)))
     (else #f))))

(define (toml->scm s)
  (define tree (parse s))
  (let loop ((tree (if (symbol? (car tree)) (list tree) tree))
             (result '())
             (current-table '())
             (inline-table-keys '())
             (array-table-index #f))
    ;; (pretty-print tree)
    (match (car tree)
      (('keyval keys 'inline-table)
       (let ((keylist (append current-table (get-keys keys))))
         (set! inline-table-keys (cons keylist inline-table-keys))
         (set! result (add-to-tree result current-table (get-keys keys) '(()) array-table-index))))
      (('keyval keys ('inline-table keyvals ...))
       (set! result
             (loop (keyword-flatten '(keyval) keyvals)
                   result
                   (get-keys keys)
                   '()
                   array-table-index)))
      (('keyval ('simple-key value)) ;; special case for key being empty string
       (let ((keylist (append current-table '(""))))
         (check-inline-table-keys keylist inline-table-keys)
         (set! result (add-to-tree result current-table '("") (list value) array-table-index))))
      (('keyval keys value ...)
       (let ((keylist (append current-table (get-keys keys))))
         (check-inline-table-keys keylist inline-table-keys)
         (set! result (add-to-tree result current-table (get-keys keys) value array-table-index))))
      (('std-table keys ...)
       (set! array-table-index #f)
       (let ((keylist (get-keys keys)))
         (check-inline-table-keys keylist inline-table-keys)
         (unless (find-key result keylist)
           (set! result (add-to-tree result '() keylist '(()) array-table-index)))
         (set! current-table keylist)))
      (('array-table keys ...)
       (set! array-table-index 0)
       (let ((keylist (get-keys keys)))
         (check-inline-table-keys keylist inline-table-keys)
         (set! current-table keylist)))




      (x (format #t "~a ; unknown: ~a\n" tree x)))

    (if (null? (cdr tree))
        result
        (loop (cdr tree)
              result
              current-table
              inline-table-keys
              array-table-index))))
