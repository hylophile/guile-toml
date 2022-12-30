(use-modules (ice-9 textual-ports))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 peg) (ice-9 match))
(use-modules (json))

(define (read-from-file file)
  (call-with-input-file file get-string-all))

(define t (read-from-file "ex.toml"))

;; (define-peg-pattern q all (#\"))

(define-peg-string-patterns
  "toml <- (line NL)* line
line <- SEP* (array/comment/kvpair/br/TEXT) SEP* comment?
br <-- LBR brkey (DOT brkey)* RBR
kvpair <- key SEP* EQ SEP* value
key <- ASCIICHAR*
brkey <-- key
value <- date/int/string/bool/array/TEXT
array <-- LBR (SEPNL* value SEPNL* COMMA)* SEPNL* value? SEPNL* RBR
string <-- QU (!QU .)* QU
bool <-- 'true'/'false'
int <-- [0-9]+
date <-- num num num num MINUS num num MINUS num num 'T' num num COLON num num COLON num num MINUS num num COLON num num
num <- [0-9]
MINUS <- '-'
COLON <- ':'
LBR < '['
RBR < ']'
COMMA < ','
comment <-- SEP* CMT SEP* (!NL .)*
CMT < '#'
inlinecomment <-- (!NL .)*
TEXT <- (!NL .)*
EXT <- (!NL .)*
SEP < ' '/'\t'
SEPNL < SEP/NL
S <- SEP*
ASCIICHAR <- [a-z_0-9]
DOT < '.'
EQ < '='
QU < '\"'
NL < '\n'")


;; (define (parse l)
;;   (keyword-flatten '(definition number op) (peg:tree (match-pattern program (string-join l "\n")))))

(define (parse s)
  (peg:tree (match-pattern toml s)))

;; (parse "x = [ 8000, 8002 ]")

(define-peg-string-patterns
  "xxx <- [a-z]
")

(define-peg-pattern rrr all (range #\x20 #\x20))

(search-for-pattern "#\x27" "'abc ")

(pretty-print (parse t))

(peg:tree (match-pattern rrr " \nien"))

;; (pretty-print (peg:substring (match-pattern toml t)))
;; (display t)

;; (pretty-print (json-string->scm "{\"best-day-ever\": {\"type\": \"datetime\", \"value\": \"1987-07-05T17:45:00Z\"},\"numtheory\": {\"boring\": {\"type\": \"bool\", \"value\": \"false\"},\"perfection\": [{\"type\": \"integer\", \"value\": \"6\"},{\"type\": \"integer\", \"value\": \"28\"},{\"type\": \"integer\", \"value\": \"496\"}]}}"))
