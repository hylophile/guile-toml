(use-modules (srfi srfi-64)
             (srfi srfi-19)
             (toml))

(test-begin "decoder-datatypes-test")

(define date (make-date 0 1 1 1 1 1 2000 0))
(define toml-datetime "datetime = 2000-01-01T01:01:01Z")
(define scm-datetime (toml->scm toml-datetime))
(test-equal scm-datetime `(("datetime" . ,date)))

(define toml-float "float = 0.1")
(define scm-float (toml->scm toml-float))
(test-equal scm-float '(("float" . 0.1)))

(define toml-nan "nan = nan")
(define scm-nan (toml->scm toml-nan))
(test-equal scm-nan '(("nan" . +nan.0)))

(define toml-inf "inf = inf")
(define scm-inf (toml->scm toml-inf))
(test-equal scm-inf '(("inf" . +inf.0)))

(define toml-positive-inf "positive-inf = +inf")
(define scm-positive-inf (toml->scm toml-positive-inf))
(test-equal scm-positive-inf '(("positive-inf" . +inf.0)))

(define toml-negative-inf "negative-inf = -inf")
(define scm-negative-inf (toml->scm toml-negative-inf))
(test-equal scm-negative-inf '(("negative-inf" . -inf.0)))

(define toml-true "true = true")
(define scm-true (toml->scm toml-true))
(test-equal scm-true '(("true" . #t)))

(define toml-false "false = false")
(define scm-false (toml->scm toml-false))
(test-equal scm-false '(("false" . #f)))

(test-end "decoder-datatypes-test")
