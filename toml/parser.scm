(define-module (toml parser)
  #:use-module (toml parser-peg)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-19)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (toml->scm peg-tree->scm flatten-tree flatten-array value->scm validate-date-time read-string read-int value?))


(define-syntax-rule (log-exprs exp ...) (begin (format #t "~a: ~S\n" (quote exp) exp) ...))

(define (flatten-array l)
  (keyword-flatten '(string bool array inline-table datetime float integer) l))

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

;; we want to be able to dynamically bind this function in test-decoder.scm
(define value->scm
  (make-parameter
   (lambda (value-pair)
     ;; (log-exprs value-pair)
     (match value-pair
       (('array value-pairs ...)
        ;; (log-exprs value-pairs (flatten-array value-pairs))
        ;; (pretty-print value-pairs)
        ;; (newline)
        ;; (pretty-print (flatten-array value-pairs))
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
        (validate-date-time `(datetime ,v))
        (datetime->date v))
       (('datetime-local v)
        (validate-date-time `(datetime-local ,v))
        (datetime-local->date v))
       (('date-local v)
        (validate-date-time `(date-local ,v))
        (date-local->date v))
       (('time-local v)
        (validate-date-time `(time-local ,v))
        (time-local->date v))
       (('inline-table "")
        '())
       (('inline-table xs ...)
        (peg-tree->scm (flatten-tree xs)))
       ((x y)
        (format #f "~a: ~a" x y))
       ('()
        '())
       ('(())
        (error "redef"))
       ;; ('inline-table
       ;;  '())
       (_ (error "err:" value-pair))))))

(define (normalize-date-time s)
  (define s-with-T (string-replace s "T" 10 11)) ;; could be t, T, or " ", so we can't just upcase all
  (define s-upcased (string-upcase s-with-T))
  (define s-without-colon (if (string-match "[+-][0-9][0-9]:[0-9][0-9]$" s)
                              (regexp-substitute #f (string-match ":([0-9][0-9])$" s-with-T) 'pre "" 1 'post)
                              s-upcased))
  s-without-colon)

(define (datetime->date s)
  (define format (string-append
                  "~Y-~m-~dT~H:~M:~S"
                  (if (string-contains s ".") ".~N" "")
                  "~z"))
  (string->date (normalize-date-time s) format))

(define (datetime-local->date s)
  (string->date (normalize-date-time s) "~Y-~m-~dT~H:~M:~S"))

(define (date-local->date s)
  (string->date s "~Y~m~d"))

(define (time-local->date s)
  (datetime-local->date (string-append "1970-01-01T" s)))

(define (remove-nanos s)
  (define nanos (string-match "\\.[0-9]+" s))
  (define region (and nanos (vector-ref nanos 1)))
  (if region
      (string-replace s "" (car region) (cdr region))
      s))

(define (validate-date-time v)
  (let ((result (match v
                  (('datetime s)
                   (strptime "%FT%T%z" (normalize-date-time (remove-nanos s))))
                  (('datetime-local s)
                   (strptime "%FT%T" (normalize-date-time (remove-nanos s))))
                  (('date-local s)
                   (strptime "%F" s))
                  (('time-local s)
                   (strptime "%T" (remove-nanos s))))))
    (when (and result (< 60 (tm:sec (car result))))
      (error "guile-toml: invalid date-time"))))

;; (define s "1987-12-21")
;; (tm:sec (car (strptime "%F" s)))
;; (validate-date-time `(datetime ,s))



;; (datetime-string->date "1987-12-21T18:23:23+08:00")
;; (car (strptime "%FT%T%z" "1987-18-83T18:28:60+08:00"))
;; (tm:gmtoff (car (strptime "%FT%T%z" "1987-12-13T18:28:60+9100")))
;; (date->string (rfc3339-string->date "1987-12-83T18:28:60+08:00"))

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

(define (find-subtree tree key)
  (let ((k (list-index (lambda (x) (equal? x key))
                       (map car tree))))
    (and k (cons k (list-ref tree k)))))

(find-subtree '(("x" ("y"))) "x")
(find-subtree '(("a" . 2) ("b" . 2)) '("c"))
;; (map car '(("x")))



(define (add-to-tree tree table-keys keys value array-table-index)
  (if array-table-index
      (add-kv-to-tree-in-array tree table-keys keys value array-table-index)
      (add-kv-to-tree tree (append table-keys keys) value)))

(define (init-array-table keys value)
  (cons (first keys) (list->vector `((,(cons (second keys) value))))))

;; adds kv to the tree within array[(length array)], always in the last position of the array
(define (add-kv-to-array array keys value index)
  (define l (vector->list array))
  (define k (1- (length l)))
  (if (eq? '() value)
      (extend-array-table array)
      (begin (list-set! l
                        k
                        (add-kv-to-tree (vector-ref array k) keys value))
             (list->vector l))))

;; (add-kv-to-array #((("a" . 2) ("b" . 2)) (("d" . 2))) '("c") '((integer "3")) 0)
(define (extend-array-table array)
  (list->vector (append (vector->list array) '(()))))



;; replaces keys at position k in tree (probably could just use list-set! here)
(define (replace-in-tree tree keyname k new)
  ;; (log-exprs "repla" tree keyname k new)
  (append
   (take tree k)
   (list (cons
          keyname
          new))
   (drop tree (1+ k))))



;; adds kv pair which is within an array table (needs a better name)
(define (add-kv-to-tree-in-array tree table-keys keys value index)
  ;; (pretty-print value)
  ;; (log-exprs tree table-keys keys ".")
  (if (null? (cdr table-keys))
      (begin
        ;; (log-exprs "/////////////" tree table-keys keys value "///////////////")
        (if (or (null? tree) (null? (car tree)))
            ;; (list (cons (car table-keys) (add-kv-to-array (if (null? tree) #(()) (cdr tree)) keys value index)))
            (list (cons (car table-keys) (make-vector 1 '())))
            (let ((k (list-index (lambda (x) (equal? x (car table-keys))) (map car tree))))
              ;; (log-exprs tree keys value k)
              (if k
                  (let ((e (list-ref tree k)))
                    (replace-in-tree tree (car e) k (add-kv-to-array (cdr e) keys value 0)))
                  (cons (cons (car table-keys) (make-vector 1 '())) tree)))))
      ;; (cons (cons (car table-keys) (add-kv-to-array #(()) keys value index)) tree)))))
      (let ((f (find-subtree tree (car table-keys))))
        ;; (log-exprs f table-keys keys value)
        (cond
         ((and f (vector? (cddr f)))
          ;; (log-exprs "-----" tree table-keys keys value f "------------")
          (let* ((k (car f)) (e (cdr f))
                 (array (cdr e))
                 (len (1- (vector-length array)))
                 (el-of-array (vector-ref array len))
                 (new-el (add-kv-to-tree-in-array el-of-array (cdr table-keys) keys value index)))
            ;; (log-exprs "~~~" array el-of-array new-el "~~~")
            (vector-set! array len new-el)
            tree))
         (f
          (let* ((k (car f)) (e (cdr f)))
            (replace-in-tree tree (car e) k (add-kv-to-tree-in-array (cdr e) (cdr table-keys) keys value index))))
         (else
          (cons (cons (car table-keys) (add-kv-to-tree-in-array '()
                                                                (cdr table-keys) keys value index))
                tree))))))

(define (add-kv-to-tree tree keys value)
  ;; (pretty-print value)
  ;; (log-exprs tree keys value)
  (let ((k (and (not (null? keys))
                (list-index (lambda (x) (equal? x (car keys))) (map car tree)))))
    ;; (log-exprs k "!")
    (if k
        (let ((e (list-ref tree k)))
          (cond
           ((vector? (cdr e))
            ;; (cons (add-kv-to-array (cdr e) keys value #f))
            (replace-in-tree tree (car e) k (add-kv-to-array (cdr e) (cdr keys) value #f)))
           (((value?) e)
            (error "guile-toml: redefinition not allowed"))
           (else
            (replace-in-tree tree (car e) k (add-kv-to-tree (cdr e) (cdr keys) value)))))
        (if (and (null? keys) (null? (car value)))
            tree
            (cons (keyval->scm keys value #f) tree)))))

;; '(a b c) -> '((a) (a b) (a b c))
(define (heads lst)
  (map (lambda (k) (list-head lst k)) (iota (length lst) 1)))

(define (check-keyval-keys keylist explicit-keyval-keys)
  ;; (log-exprs keylist explicit-keyval-keys)
  (let ((heads (map (lambda (k) (list-head keylist k))
                    (iota (length keylist) 1))))
    (when (any (lambda (x) (member x explicit-keyval-keys))
               heads)
      (begin
        (error "guile-toml: redefinition not allowed" keylist explicit-keyval-keys)))))

;; a drop that doesn't error out when k is too big
(define (safe-drop lst k)
  (match (false-if-exception (drop lst k))
    (#f '())
    (x x)))

;; (define* (check-explicit-table-keys keylist explicit-table-keys #:optional (current-table-length 0))
;;   (when (and (not (null? keylist))
;;              (or
;;               (member keylist explicit-table-keys)
;;               (any (lambda (x) (member x explicit-table-keys)) (safe-drop (heads keylist) current-table-length))))
;;     (error "guile-toml: redefinition not allowed" keylist explicit-table-keys current-table-length)))

;; we check all heads of keylist, and drop the keys containing only the current table before
(define* (check-explicit-table-keys keylist explicit-table-keys #:optional (current-table-length 0))
  ;; (log-exprs keylist explicit-table-keys current-table-length "____")
  (when (and (not (null? keylist))
             (or
              (member keylist explicit-table-keys)
              (any (lambda (x) (member x explicit-table-keys)) (safe-drop (heads keylist) current-table-length))))
    (error "guile-toml: redefinition not allowed" keylist explicit-table-keys current-table-length)))

;; (check-explicit-table-keys '("x") '(("x" "c" "s")) 2)
(define (all-keys-but-last l) (reverse (cdr (reverse l))))

;; (define (car-or-last-of-vector lv)
;;   (if (vector? lv)
;;       (vector-ref lv (vector-length lv))
;;       (car lv)))
(define (tree-or-last-of-vector tree)
  (if (vector? tree)
      (vector-ref tree (1- (vector-length tree)))
      tree))

(define (find-key tree keys)
  ;; (log-exprs tree keys)
  (let* ((tree (tree-or-last-of-vector tree))
         (k (list-index (lambda (x) (equal? x (car keys)))
                        (map car tree))))
    (cond
     ((and k (null? (cdr keys))) (list-ref tree k))
     (k (find-key (cdr (list-ref tree k)) (cdr keys)))
     (else #f))))

(define (toml->scm s)
  (define tree (parse s))
  (peg-tree->scm (flatten-tree tree)))

(define (flatten-tree tree)
  (keyword-flatten '(keyval array-table std-table inline-table) tree))

;; could refactor to have one 'keyval match, then match the rest again
(define (peg-tree->scm tree)
  ;; (pretty-print tree)
  (let loop ((tree (cond ((null? tree) '(()))
                         ((symbol? (car tree)) (list tree))
                         (else tree)))
             (result '())
             (current-table '())
             (explicit-keyval-keys '())
             (explicit-table-keys '())
             (explicit-array-table-keys '())
             (new-explicit-table-keys '())
             (array-table-index #f))
    (match (car tree)
      (('keyval ('simple-key value)) ;; special case for key being empty string
       (let* ((keylist (append current-table '(""))))
         (if (not array-table-index)
             (check-keyval-keys keylist explicit-keyval-keys))
         (set! result (add-to-tree result current-table '("") (list value) array-table-index))))

      (('keyval keys value ...)
       (let* ((keylist (append current-table (get-keys keys)))
              (keylist-all-but-last (all-keys-but-last keylist)))
         (when (not (eq? 1 (length (get-keys keys))))
           (check-explicit-table-keys keylist-all-but-last explicit-table-keys (length current-table))
           (set! new-explicit-table-keys (cons keylist-all-but-last new-explicit-table-keys)))
         (set! explicit-keyval-keys (cons keylist explicit-keyval-keys))
         (set! result (add-to-tree result current-table (get-keys keys) value array-table-index))))

      (('std-table keys ...)
       (let ((keylist (get-keys keys)))
         (set! explicit-table-keys (append new-explicit-table-keys explicit-table-keys))
         (set! new-explicit-table-keys '())
         (if (not array-table-index)
             (check-keyval-keys keylist explicit-keyval-keys))
         (if array-table-index
             (when (find-key result keylist)
               (check-explicit-table-keys keylist
                                          (append explicit-array-table-keys explicit-table-keys)
                                          (length current-table)))
             (check-explicit-table-keys keylist
                                        (append explicit-array-table-keys explicit-table-keys)
                                        (length current-table)))
         (set! array-table-index #f)
         (set! explicit-table-keys (cons keylist explicit-table-keys))
         ;; (log-exprs array-table-index explicit-table-keys)
         ;; (unless (find-key result keylist)
         (set! result (add-to-tree result '() keylist '(()) array-table-index))
         (set! current-table keylist)))

      (('array-table keys ...)
       (set! array-table-index 0)
       (let ((keylist (get-keys keys)))
         (check-explicit-table-keys keylist (remove (lambda (x) (equal? x keylist)) explicit-table-keys))
         (set! explicit-array-table-keys (cons keylist explicit-array-table-keys))
         (set! current-table keylist)
         (set! result (add-to-tree result current-table '() '() array-table-index))))
      ('() '())
      (x (error x)))

    (if (null? (cdr tree))
        result
        (loop (cdr tree)
              result
              current-table
              explicit-keyval-keys
              explicit-table-keys
              explicit-array-table-keys
              new-explicit-table-keys
              array-table-index))))
