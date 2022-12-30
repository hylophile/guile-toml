#!/usr/bin/env -S guile -s
!#
(use-modules
 (toml)
 (ice-9 pretty-print)
 (ice-9 textual-ports))

(define str (get-string-all (current-input-port)))
(define scm (toml->scm str))

(pretty-print scm)
