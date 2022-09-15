(import (chicken base)
        (chicken condition)
        (chicken random)
        (srfi 134)
        (srfi 134 extensions)
        (only (srfi 1) take drop iota split-at list-tabulate any every)
        (only (srfi 41) stream-null stream-null? list->stream
                        stream->list)
        test-generative
        (test))

(include "ideque-tests.scm")
(test-exit)
