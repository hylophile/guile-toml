;; Overall Structure
(use-modules (ice-9 peg))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 peg) (ice-9 match))
(use-modules (json))

(define (read-from-file file)
  (call-with-input-file file get-string-all))

(define t (read-from-file "ex.toml"))

(define (parse s)
  (peg:tree (match-pattern toml s)))

;; Built-in ABNF terms, reproduced here for clarity
(define-peg-string-patterns
  "ALPHA <- [A-Z] / [a-z]
DIGIT <- [0-9]
HEXDIG <- DIGIT / [A-Fa-f]
")
;; T-Newline
(define-peg-string-patterns
  "t-newline < '\n' / '\r\n'")

(define-peg-string-patterns
  "toml <- (t-expression t-newline)* t-expression
t-expression <- ((ws keyval ws) / (ws table ws) / ws) comment?
")

;; Whitespace
(define-peg-string-patterns
  "ws < wschar*
wschar < ' ' / '\t'
")
;; Comment
;; non-ascii <- %x80-D7FF / %xE000-10FFFF
;; non-eol <- %x09 / %x20-7F / non-ascii

(define-peg-pattern non-ascii body
  (or (range #\x80 #\xD7FF) (range #\xE000 #\x10FFFF)))
(define-peg-pattern non-eol body
  (or "\t" (range #\x20 #\x7F) non-ascii))

(define-peg-string-patterns
  "comment-start-symbol <- '#'

comment < comment-start-symbol non-eol*
")
;; Key-Value pairs

(define-peg-string-patterns
  "keyval <-- key keyval-sep val
key <- dotted-key / simple-key
simple-key <-- quoted-key / unquoted-key

unquoted-key <- (ALPHA / DIGIT / '-' / '_')+
quoted-key <- basic-string / literal-string
dotted-key <- simple-key ( dot-sep simple-key )+

dot-sep   <- ws dot ws
dot < '.'
keyval-sep <- ws eq ws
eq < '='
val <- string / boolean / array / inline-table / date-time / float / integer

")
;; String
(define-peg-string-patterns
  "string <-- ml-basic-string / basic-string / ml-literal-string / literal-string
")
;; Basic String
(define-peg-string-patterns
  "basic-string <- quotation-mark basic-char* quotation-mark

quotation-mark < '\"'

basic-char <- basic-unescaped / escaped
")

;; basic-unescaped <- wschar / %x21 / %x23-5B / %x5D-7E / non-ascii
(define-peg-pattern basic-unescaped body
  (or body-wschar (range #\x21 #\x21) (range #\x23 #\x5B) (range #\x5D #\x7E) non-ascii))
(define-peg-string-patterns
  "escaped <-- escape escape-seq-char

escape <- '\\'
")

(define-peg-pattern escape-seq-char body
  (or
   "\""
   "\\"
   "\b"
   "\f"
   "\n"
   "\r"
   "\t"
   (and (range #\u #\u) HEXDIG HEXDIG HEXDIG HEXDIG)
   (and (range #\U #\U) HEXDIG HEXDIG HEXDIG HEXDIG HEXDIG HEXDIG HEXDIG HEXDIG)))

;; escape-seq-char <-  %x22         ;     quotation mark  U+0022
;; escape-seq-char =/ %x5C         ;     reverse solidus U+005C
;; escape-seq-char =/ %x62         ; b    backspace       U+0008
;; escape-seq-char =/ %x66         ; f    form feed       U+000C
;; escape-seq-char =/ %x6E         ; n    line feed       U+000A
;; escape-seq-char =/ %x72         ; r    carriage return U+000D
;; escape-seq-char =/ %x74         ; t    tab             U+0009
;; escape-seq-char =/ %x75 4HEXDIG ; uXXXX                U+XXXX
;; escape-seq-char =/ %x55 8HEXDIG ; UXXXXXXXX            U+XXXXXXXX
;; ))




;; Multiline Basic String
(define-peg-string-patterns
  "ml-basic-string <- ml-basic-string-delim t-newline? ml-basic-body ml-basic-string-delim
ml-basic-string-delim <- quotation-mark quotation-mark quotation-mark
ml-basic-body <- mlb-content* (mlb-quotes mlb-content+)* mlb-quotes?

body-newline <- '\n' / '\r\n'
body-ws <- body-wschar*
body-wschar <- ' ' / '\t'
body-quot <- '\"'

mlb-content <- mlb-escaped-nl / mlb-char / body-newline
mlb-char <- mlb-unescaped / escaped
mlb-quotes <- !ml-basic-string-delim body-quot body-quot?
mlb-escaped-nl < escape ws body-newline (wschar / body-newline)*
")
;; mlb-unescaped <- wschar / %x21 / %x23-5B / %x5D-7E / non-ascii
(define-peg-pattern mlb-unescaped body
  (or body-wschar (range #\x21 #\x21) (range #\x23 #\x5B) (range #\x5D #\x7E) non-ascii))

;; Literal String
(define-peg-string-patterns
  "literal-string <- apostrophe literal-char* apostrophe
")

;; apostrophe <- '\x27' ; apostrophe
;; literal-char <- %x09 / %x20-26 / %x28-7E / non-ascii
(define-peg-pattern apostrophe none "'")
(define-peg-pattern body-apostrophe body "'")
(define-peg-pattern literal-char body
  (or "\t" (range #\x20 #\x26) (range #\x28 #\x7E) non-ascii))

;; Multiline Literal String
(define-peg-string-patterns
  "mll-quotes <- !ml-literal-string-delim body-apostrophe body-apostrophe?

ml-literal-string <- ml-literal-string-delim t-newline? ml-literal-body ml-literal-string-delim
ml-literal-string-delim <- apostrophe apostrophe apostrophe
ml-literal-body <- mll-content* (mll-quotes mll-content+ )* mll-quotes?

mll-content <- mll-char / body-newline
")

(define-peg-pattern mll-char body
  (or "\t" (range #\x20 #\x26) (range #\x28 #\x7E) non-ascii))
;; Integer
(define-peg-string-patterns
  "integer <- hex-int / oct-int / bin-int / dec-int

minus <- '-'
plus <- '+'
underscore < '_'
digit1-9 <- [1-9]
digit0-7 <- [0-9]
digit0-1 <- [0-1]

hex-prefix <- '0x'
oct-prefix <- '0o'
bin-prefix <- '0b'

dec-int <-- (minus / plus)? unsigned-dec-int
unsigned-dec-int <- (digit1-9 ( DIGIT / (underscore DIGIT))+) / DIGIT

hex-int <-- hex-prefix HEXDIG (HEXDIG / underscore HEXDIG)*
oct-int <-- oct-prefix digit0-7 (digit0-7 / underscore digit0-7)*
bin-int <-- bin-prefix digit0-1 (digit0-1 / underscore digit0-1)*
")
;; Float
(define-peg-string-patterns
  "float <-- (float-int-part ( t-exp / frac t-exp?)) / special-float

float-dec-int <- (minus / plus)? unsigned-dec-int
float-int-part <- float-dec-int
frac <- decimal-point zero-prefixable-int
decimal-point <- '.'
zero-prefixable-int <- DIGIT (DIGIT / underscore DIGIT)*

t-exp <- [eE] float-t-exp-part
float-t-exp-part <- (minus / plus)? zero-prefixable-int

special-float <-- (minus / plus)? ( t-inf / t-nan)
t-inf <- 'inf'
t-nan <- 'nan'
")
;; Boolean
(define-peg-string-patterns
  "boolean <-- 'true' / 'false'

")
;; true    <- %x74.72.75.65     ; true
;; false   <- %x66.61.6C.73.65  ; false

;; Date and Time (as defined in RFC 3339)
(define-peg-string-patterns
  "date-time      <- offset-date-time / local-date-time / local-date / local-time

date-fullyear  <- DIGIT DIGIT DIGIT DIGIT
date-month     <- DIGIT DIGIT
date-mday      <- DIGIT DIGIT
time-delim     <- 'T' / ' '
time-hour      <- DIGIT DIGIT
time-minute    <- DIGIT DIGIT
time-second    <- DIGIT DIGIT
time-secfrac   <- '.' DIGIT+
time-numoffset <- ( '+' / '-' ) time-hour ':' time-minute
time-offset    <- 'Z' / time-numoffset

partial-time   <- time-hour ':' time-minute ':' time-second time-secfrac?
full-date      <- date-fullyear '-' date-month '-' date-mday
full-time      <- partial-time time-offset
")
;; time-delim     <- 'T' / ' ' ; T, t, or space

;; Offset Date-Time
(define-peg-string-patterns
  "offset-date-time <-- full-date time-delim full-time
")
;; Local Date-Time
(define-peg-string-patterns
  "local-date-time <-- full-date time-delim partial-time
")
;; Local Date
(define-peg-string-patterns
  "local-date <-- full-date
")
;; Local Time
(define-peg-string-patterns
  "local-time <-- partial-time
")
;; Array
;; array-values <- ws-comment-t-newline val ws-comment-t-newline array-sep array-values / ws-comment-t-newline val ws-comment-t-newline array-sep?
(define-peg-string-patterns
  "array <-- array-open array-values? ws-comment-t-newline array-close

array-open < '['
array-close < ']'

array-values <- (ws-comment-t-newline val ws-comment-t-newline array-sep)* ws-comment-t-newline val ws-comment-t-newline array-sep?

array-sep < ','

ws-comment-t-newline <- ((comment? t-newline) / wschar)*
")
;; Table
(define-peg-string-patterns
  "table <- std-table / array-table
")
;; Standard Table
(define-peg-string-patterns
  "std-table <-- std-table-open key std-table-close

std-table-open  < '[' ws
std-table-close < ws ']'
")
;; Inline Table
(define-peg-string-patterns
  "inline-table <- inline-table-open inline-table-keyvals? inline-table-close

inline-table-open  < '{' ws
inline-table-close < ws '}'
inline-table-sep   < ws ',' ws

inline-table-keyvals <- keyval (inline-table-sep inline-table-keyvals)?
")
;; Array Table
(define-peg-string-patterns
  "array-table <-- array-table-open key array-table-close

array-table-open  < '[[' ws
array-table-close < ws ']]'
")


(define x (match-pattern toml t))
;; (pretty-print (keyword-flatten '(simple-key array keyval std-table) (peg:tree x)))

;; (display "\n\n")

;; (display (peg:tree x))
(define x (keyword-flatten '(simple-key array keyval std-table) (peg:tree x)))


(define (get-keys l)
  (map cadr (keyword-flatten '(simple-key) l)))

(define (keys->string l)
  (string-join (get-keys l) "."))

(define (get-values l)
  (match l
    (('array vs ...) (keyword-flatten '(dec-int float string bola) l))
    ((_ x) x)))

(get-values '(array (a b)))

(define (values->string l)
  (match (car l)
    (('array vs ...) vs)
    (x (cadr x))))

(define (value->string v)
  (cdr v))

(define (a)
  (let loop ((tree x))
    (match (car tree)
      (('keyval keys values ...)
       (format #t "~a = ~S\n" (keys->string keys) (values->string values)))
      (('std-table keys ...)
       (format #t "[~a]\n" (keys->string keys)))
      ((x ...) (display x)))

    (unless (null? (cdr tree))
      (loop (cdr tree)))))


(a)

;; (display (scm->json (peg:tree x)))
;; (pretty-print (peg:substring x))
(keyword-flatten '(simple-key) '((simple-key hosts) ((simple-key l) (simple-key ol))))
(keyword-flatten '(simple-key) '(simple-key hosts))
