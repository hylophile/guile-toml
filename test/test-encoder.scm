#!/usr/bin/env -S guile -s
!#
(use-modules
 (toml parser)
 (json)
 (ice-9 match)
 (ice-9 textual-ports)
 (ice-9 pretty-print))

(set-port-conversion-strategy! (current-input-port) 'error)

;; (define str (get-string-all (current-input-port)))

(define scm (json->scm (current-input-port)))

(display scm)
