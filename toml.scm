(define-module (toml)
  #:use-module (toml parser)
  #:use-module (ice-9 peg)
  ;; #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (srfi srfi-1))


;; (define (a)
;;   (let loop ((tree x))
;;     (match (car tree)
;;       (('keyval keys values ...)
;;        (format #t "~a = ~S\n" (keys->string keys) (values->string values)))
;;       (('std-table keys ...)
;;        (format #t "[~a]\n" (keys->string keys)))
;;       ((x ...) (display x)))

;;     (unless (null? (cdr tree))
;;       (loop (cdr tree)))))



(define (read-from-file file)
  (call-with-input-file file get-string-all))

(define t (read-from-file "spec-example-1-compact.toml"))

;; (define (parse s)
;;   (peg:tree (match-pattern toml s)))

;; (define x (match-pattern toml t))
;; (pretty-print (keyword-flatten '(simple-key array keyval std-table) (peg:tree x)))

;; (display "\n\n")

;; (display (peg:tree x))
;; (define x (keyword-flatten '(simple-key array keyval std-table) (peg:tree (match-pattern toml t))))

(define (flatten-array l)
  (keyword-flatten '(array dec-int float string bola) l))


(define (get-keys l)
  (map cadr (keyword-flatten '(simple-key) l)))

;; (define (keys->string l)
;;   (string-join (get-keys l) "."))

;; (define (get-values l)
;;   (match l
;;     (('array vs ...) (keyword-flatten '(dec-int float string bola) vs))
;;     ((_ x) x)))

;; (get-values '(((a b) (c d)) (e f)))

;; (define (values->string l)
;;   (match (car l)
;;     (('array vs ...) vs)
;;     (x (cadr x))))

;; (define (value->string v)
;;   (cdr v))

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
    (_ (format #f "err: ~a" v))))

(define (keyval->scm keys value)
  (let loop ((keys keys))
    (if (null? (cdr keys))
        (cons (car keys) (value->scm (car value)))
        (list (car keys) (loop (cdr keys))))))


;; (define tr '(("a" ("aa" . "v")) ("b" "c")))

;; (list-index (lambda (x) (equal? x "c")) (map car tr))

(define (add-to-tree tree keys value)
  (if (null? keys)
      ;; TODO helper to never call this on top-level
      (value->scm value)
      (let* ((k (list-index (lambda (x) (equal? x (car keys))) (map car tree))))
        (if k
            (let ((e (list-ref tree k)))
              (unless (list? e)
                (error "guile-toml: redefinition not allowed"))
              (append
               (take tree k)
               (list (cons
                      (car e)
                      (add-to-tree (cdr e) (cdr keys) value)))
               (drop tree (1+ k))))
            ;; (receive (a b) (split-at tree k)
            ;;   (display a)
            ;;   (newline)
            ;;   (display b)
            ;;   (newline)
            ;;   (display (cdr b))
            ;;   (newline)
            ;;   (newline)
            ;;   (if (list? (car b))

            ;;       ;; (append a
            ;;       ;;         (cons
            ;;       ;;          (caar b)
            ;;       ;;          (add-to-tree (cdar b) (cdr keys) value))
            ;;       ;;         (cdr b))
            ;;       (error "redefinition")))
            (cons (keyval->scm keys value) tree)))))

;; (add-to-tree '(("a" ("e" . "hi")) ("b" . 3)) '("a" "e") "c")

;; (add-to-tree '(("z")("a")) '("a") "c")
;; (add-to-tree '() '("a") "c")

;; (map car '(("a" . "d")))

;; (define x '(1 2 3 4))
;; (define n 2)
;; (append (take x n) (list (list-ref x n)) (drop x (1+ n)))


;; (json-string->scm "{\"b\": {\"e\":\"c\"}}")
;; (json-string->scm "{\"b\": {}}")
;; (list-set! tree k (add-to-tree (list-ref tree k) (cdr keys) value)
;;             tree))))))


;; (receive (c d) (split-at '(a b) 1) (display c))

;; (keyval->scm '(simple-key "a") '(string "a"))

(define (b s)
  (let loop ((tree (keyword-flatten
                    '(simple-key array keyval std-table)
                    (parse s)))
             (result '())
             (current-table '()))
    (match (car tree)
      (('keyval keys value ...)
       ;; (pretty-print (keyval->scm (append current-table (get-keys keys)) value))
       (set! result (add-to-tree result (append current-table (get-keys keys)) value)))
      ;; (pretty-print (get-keys keys))
      ;; (pretty-print keys)
      ;; (pretty-print value))
      ;; (display "\n"))
      ;; (format #t "~a = ~S\n" (keys->string keys) (values->string values)))
      (('std-table keys ...)
       ;; (format #t "[~a]\n" (keys->string keys))
       (set! current-table (get-keys keys)))
      ((x ...) (display "WTF")(display x)))

    (if (null? (cdr tree))
        result
        (loop (cdr tree) result current-table))))

;; (let ((single-value-proc annot-v-proc
;;                 (b))))

;; (pretty-print (let ((single-value-proc annot-v-proc))
;;                 (b)))

;; (display "\n\n\n")

(display (scm->json (b (get-string-all (current-input-port)))))
(newline)

;; (pretty-print (json-string->scm (call-with-input-file "spec-example-1-compact.json" get-string-all)))

;; (display (scm->json (peg:tree x)))
;; (pretty-print (peg:substring x))
;; (keyword-flatten '(simple-key) '((simple-key hosts) ((simple-key l) (simple-key ol))))
;; (keyword-flatten '(simple-key) '(simple-key hosts))
