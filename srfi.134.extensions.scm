(module (srfi 134 extensions)
  (ideque-pop-front ideque-pop-back
   ideque->stream stream->ideque
   )

(import scheme
        (only (chicken base) include unless)
        (chicken condition)
        (chicken type)
        (srfi 41)
        (srfi 134 internal))

(include "exceptions.scm")

;;;; "Crossed" accessors

(: ideque-pop-front (ideque -> * ideque))
(define (ideque-pop-front dq)
  (assert-type 'ideque-pop-front (ideque? dq))
  (if (zero? (dq-lenf dq))
      (if (zero? (dq-lenr dq))
          (bounds-exception 'ideque-pop-front "empty deque" dq)
          (values (stream-car (dq-r dq)) the-empty-ideque))
      (let ((f (dq-f dq)))
        (values (stream-car f)
                (make-deque (- (dq-lenf dq) 1)
                            (stream-cdr f)
                            (dq-lenr dq)
                            (dq-r dq))))))

(: ideque-pop-back (ideque -> * ideque))
(define (ideque-pop-back dq)
  (assert-type 'ideque-pop-back (ideque? dq))
  (if (zero? (dq-lenr dq))
      (if (zero? (dq-lenf dq))
          (bounds-exception 'ideque-pop-back "empty deque" dq)
          (values (stream-car (dq-f dq)) the-empty-ideque))
      (let ((r (dq-r dq)))
        (values (stream-car r)
                (make-deque (dq-lenf dq)
                            (dq-f dq)
                            (- (dq-lenr dq) 1)
                            (stream-cdr r))))))

;;;; Stream conversions

(: ideque->stream (ideque -> *))
(define (ideque->stream dq)
  (assert-type 'ideque->stream (ideque? dq))
  (stream-append (dq-f dq) (stream-reverse (dq-r dq))))

(: stream->ideque (* -> ideque))
(define (stream->ideque stream)
  (make-deque (stream-length stream)
              stream
              0
              stream-null))

)
