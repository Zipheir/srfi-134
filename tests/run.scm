(import (chicken base)
        (chicken condition)
        (chicken random)
        (srfi 134)
        (only (srfi 1) take drop iota split-at list-tabulate)
        (only (srfi 41) stream-null stream-null? list->stream
                        stream->list)
        test-generative
        (test))

(include "ideque-tests.scm")
(test-exit)
