(define-module (toml builder)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 receive)
  #:export (scm->toml))


(define-syntax-rule (log-exprs exp ...) (begin (format #t "~a: ~S\n" (quote exp) exp) ...))

;; we want to be able to dynamically bind this functin in test-decoder.scm
;; TODO fix duplicate
(define value?
  (make-parameter
   (lambda (expr) (not (list? expr)))))

;; (define scm->value
;;   (lambda))

(define (build-object-pair p port)
  ;; (log-exprs p)
  ;; (put-string port "pair:")
  (put-string port (car p))
  (put-string port " = ")
  ;; (put-string port (cdr p))
  ;; (newline port)
  ;; (put-string port (indent-string pretty level))
  ;; (json-build-string (car p) port solidus unicode)
  ;; (put-string port ":")
  ;; (build-space port pretty)
  (toml-build (cdr p) port))

(define (toml-build-string s port)
  (put-string port "\"")
  (put-string port s)
  (put-string port "\"")
  (newline port))

;; (define (values-first a b)
;;   (let ((av? ((value?) a))
;;         (bv? ((value?) b)))
;;    (cond
;;     ((and av? bv?) #t)
;;     (av? #t)
;;     (bv? #f))))

(define (build-table scm port)
  (put-string port "[")
  (put-string port (car scm))
  (put-string port "]")
  (newline port)
  (toml-build (cdr scm) port))

(define (toml-build-object scm port)
  (let ((pairs scm))
    (unless (null? pairs)
      (receive (keyvals tables)
          (partition (value?) pairs)
        (for-each (lambda (kv)
                    (build-object-pair kv port))
                  keyvals)
        (for-each (lambda (t)
                    (build-table t port))
                  tables)))))

;; (build-object-pair (car pairs) port)
;; (for-each (lambda (p)
;;             (build-object-pair p port))
;;           (cdr pairs))
;; (newline port))))

(define (toml-build scm port)
  ;; (log-exprs scm)
  (cond
   ;; ((eq? scm null) (toml-build-null port))
   ;; ((boolean? scm) (toml-build-boolean scm port))
   ;; ((toml-number? scm) (toml-build-number scm port))
   ;; ((symbol? scm) (toml-build-string (symbol->string scm) port))
   ((string? scm) (toml-build-string scm port))
   ;; ((vector? scm) (toml-build-array scm port))
   ((or (pair? scm) (null? scm))
    (toml-build-object scm port))))
;; (else (throw 'toml-invalid scm))))

(define* (scm->toml scm
                    #:optional (port (current-output-port)))
  (toml-build scm port))

;; (scm->toml '(("a" . "b") ("c" . "d")))
;; (scm->toml '(("yo" ("a" . "b"))))
;; (scm->toml '(("yo" ("a" . "b") ("c" . "d")) ("e" . "f")))

'(("servers"
   ("beta" ("role" . "backend") ("ip" . "10.0.0.2"))
   ("alpha"
    ("role" . "frontend")
    ("ip" . "10.0.0.1")))
  ("database"
   ("temp_targets" ("case" . 72.0) ("cpu" . 79.5))
   ("data" . #(#("delta" "phi") #(3.14)))
   ("ports" . #(8000 8001 8002))
   ("enabled" . #t))
  ("owner"
   ("dob"
    . "date")
   ("name" . "Tom Preston-Werner"))
  ("title" . "TOML Example"))

'()
