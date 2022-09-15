;;;; See LICENSE for copyright information.

(module (srfi 134 internal)
  (ideque? dq-lenf dq-f dq-lenr dq-r the-empty-ideque make-deque)

(import (scheme)
        (only (chicken base) define-constant)
        (chicken type)
        (srfi 41)
        (typed-records))

(begin
;;;; The core ideque type

(define-record-type <ideque> (%make-dq lenf f lenr r) ideque?
  (lenf dq-lenf : fixnum)  ; length of front chain
  (f    dq-f    : *)       ; front chain
  (lenr dq-lenr : fixnum)  ; length of rear chain
  (r    dq-r    : *))      ; rear chain

;; We use a singleton for empty deque
(: the-empty-ideque (struct <ideque>))
(define the-empty-ideque (%make-dq 0 stream-null 0 stream-null))

;; Internal constructor.  Returns a new ideque, with balancing 'front' and
;; 'rear' chains.

;; Front/back stream length differential factor.
(: stream-length-factor fixnum)
(define-constant stream-length-factor 3)

(: make-deque (fixnum * fixnum * -> (struct <ideque>)))
(define (make-deque lenf f lenr r)
  (cond ((> lenf (+ (* lenr stream-length-factor) 1))
         (let* ((i (quotient (+ lenf lenr) 2))
                (j (- (+ lenf lenr) i))
                (f. (stream-take i f))
                (r. (stream-append
                     r
                     (stream-reverse (stream-drop i f)))))
           (%make-dq i f. j r.)))
        ((> lenr (+ (* lenf stream-length-factor) 1))
         (let* ((j (quotient (+ lenf lenr) 2))
                (i (- (+ lenf lenr) j))
                (r. (stream-take j r))
                (f. (stream-append
                     f
                     (stream-reverse (stream-drop j r)))))
           (%make-dq i f. j r.)))
        (else (%make-dq lenf f lenr r))))
))
