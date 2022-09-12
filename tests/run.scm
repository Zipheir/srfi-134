(import (srfi 134)
        (only (srfi 1) take drop iota split-at)
        (only (srfi 41) stream-null stream-null? list->stream
                        stream->list)
        (test))

(include "ideque-tests.scm")
(test-exit)
