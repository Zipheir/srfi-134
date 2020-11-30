(import (srfi 134)
        (only (srfi 1) take drop iota split-at)
        (only (srfi 158) generator generator->list)
        (test))

(include "ideque-tests.scm")
(test-exit)
