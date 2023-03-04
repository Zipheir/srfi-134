(module (srfi 134 extensions)
  (ideque-pop-front ideque-pop-back
   ideque-rotate
   ideque->stream stream->ideque
   )

(import scheme
        (only (chicken base) fixnum? include unless case-lambda)
        (chicken condition)
        (chicken type)
        (srfi 41)
        (srfi 134 internal))

(define-type ideque (struct <ideque>))

(include "exceptions.scm")

;;;; Utility

(: natural-fixnum? (* --> boolean))
(define (natural-fixnum? x)
  (and (fixnum? x) (not (negative? x))))

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

;;;; Rotation

;; TODO: Tune.
(: ideque-rotate (ideque #!optional fixnum -> ideque))
(define (ideque-rotate dq count)
  (assert-type 'ideque-rotate (ideque? dq))
  (assert-type 'ideque-rotate (fixnum? count))
  (stream-ref (stream-iterate (if (positive? count)
                                  %rotate-left-single
                                  %rotate-right-single)
                              dq)
              (abs count)))

(: %rotate-left-single (ideque -> ideque))
(define (%rotate-left-single dq)
  (let ((lenf (dq-lenf dq))
        (front (dq-f dq)))
    (if (zero? (dq-lenf dq))
        dq  ; null or singleton
        (make-deque (- lenf 1)
                    (stream-cdr front)
                    (+ (dq-lenr dq) 1)
                    (stream-cons (stream-car front) (dq-r dq))))))

(: %rotate-right-single (ideque -> ideque))
(define (%rotate-right-single dq)
  (let ((lenr (dq-lenr dq))
        (rear (dq-r dq)))
    (if (zero? (dq-lenr dq))
        dq  ; null or singleton
        (make-deque (+ (dq-lenf dq) 1)
                    (stream-cons (stream-car rear) (dq-f dq))
                    (- lenr 1)
                    (stream-cdr rear)))))

;;;; Stream conversions

(: ideque->stream (ideque -> *))
(define (ideque->stream dq)
  (assert-type 'ideque->stream (ideque? dq))
  (stream-append (dq-f dq) (stream-reverse (dq-r dq))))

(: stream->ideque (* -> ideque))
(define (stream->ideque stream)
  (assert-type 'stream->ideque (stream? stream))
  (make-deque (stream-length stream)
              stream
              0
              stream-null))

)
