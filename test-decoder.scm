#!/usr/bin/env -S guile -s
!#
(use-modules (json) (toml) (ice-9 textual-ports))

(define scm (toml->scm (get-string-all (current-input-port))))

(define json (scm->json scm #:pretty #t))
