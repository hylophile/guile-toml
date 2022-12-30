#!/usr/bin/env -S guile -s
!#
(use-modules
 (toml builder)
 (json))

(define scm (json->scm (current-input-port)))

(scm->toml scm)
