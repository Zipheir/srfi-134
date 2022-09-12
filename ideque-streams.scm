;;;  Copyright (c) 2015  Shiro Kawai  <shiro@acm.org>
;;;  Copyright © 2022 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation files
;;;  (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify, merge,
;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;  and to permit persons to whom the Software is furnished to do so,
;;;  subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.</p>
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;;  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;  SOFTWARE
;;;

;; This implements banker's deque as described in
;; Chris Okasaki's Purely Functional Data Structures.
;; It provides amortized O(1) basic operations.
;; Original two-list version written by Shiro Kawai.
;; Stream version by Wolfgang Corcoran-Mathe.

;;; Note on types: Streams are always given the type *, since the
;;; srfi-41 egg's types files don't define a stream type.  Oh well.

;; R7RS shim
(cond-expand
  (chicken (begin (define (eof-object) #!eof))))

;;;; Stream utility

(define (stream=? elt= s1 s2)
  (if (stream-null? s1)
      (stream-null? s2)
      (and (stream-pair? s2)
           (elt= (stream-car s1) (stream-car s2))
           (stream=? elt= (stream-cdr s1) (stream-cdr s2)))))

(: stream-count (procedure * -> integer))
(define (stream-count pred s)
  (stream-fold (lambda (n x) (if (pred x) (+ n 1) n))
               0
               s))

(: stream-filter-map (procedure * -> *))
(define stream-filter-map
  (stream-lambda (proc s)
    (cond ((stream-null? s) stream-null)
          ((proc (stream-car s)) =>
           (lambda (x)
             (stream-cons x (stream-filter-map proc (stream-cdr s)))))
          (else (stream-filter-map proc (stream-cdr s))))))

;; From SRFI 41. Clever!
(: stream-partition (procedure * -> * *))
(define (stream-partition pred s)
  (stream-unfolds
   (lambda (s)
     (if (stream-null? s)
         (values s '() '())
         (let ((a (stream-car s)) (s* (stream-cdr s)))
           (if (pred a)
               (values s* (list a) #f)
               (values s* #f (list a))))))
   s))

;; Could be improved.
(: stream-span (procedure * -> * *))
(define (stream-span pred s)
  (values (stream-take-while pred s) (stream-drop-while pred s)))

(: stream-break (procedure * -> * *))
(define (stream-break pred s)
  (stream-span (lambda (x) (not (pred x))) s))

(: stream-any (procedure * -> *))
(define (stream-any pred s)
  (let lp ((s s))
    (cond ((stream-null? s) #f)
          ((pred (stream-car s)))
          (else (lp (stream-cdr s))))))

(: stream-every (procedure * -> *))
(define (stream-every pred s)
  (let lp ((s s) (last-val #t))
    (cond ((stream-null? s) last-val)
          ((pred (stream-car s)) =>
           (lambda (x) (lp (stream-cdr s) x)))
          (else #f))))

;; Compare two streams up to whichever shorter one.
;; Returns the compare result and the tails of uncompared streams.
(: stream-prefix= (procedure * * -> boolean * *))
(define (stream-prefix= elt= a b)
  (let loop ((a a) (b b))
    (cond ((or (stream-null? a) (stream-null? b)) (values #t a b))
          ((elt= (stream-car a) (stream-car b))
           (loop (stream-cdr a) (stream-cdr b)))
          (else (values #f a b)))))

;; Compare two streams for equality using 'elt=' to compare elements.
(: stream=? (procedure * * -> boolean))
(define (stream=? elt= s1 s2)
  (if (stream-null? s1)
      (stream-null? s2)
      (and (stream-pair? s2)
           (elt= (stream-car s1) (stream-car s2))
           (stream=? elt= (stream-cdr s1) (stream-cdr s2)))))

;;;; ideque type

(define-record-type <ideque> (%make-dq lenf f lenr r) ideque?
  (lenf dq-lenf : fixnum)  ; length of front chain
  (f    dq-f    : *)       ; front chain
  (lenr dq-lenr : fixnum)  ; length of rear chain
  (r    dq-r    : *))      ; rear chain

(define-type ideque (struct <ideque>))

;; We use a singleton for empty deque
(: *empty* ideque)
(define *empty* (%make-dq 0 stream-null 0 stream-null))

;; Internal constructor.  Returns a new ideque, with balancing 'front' and
;; 'rear' chains.

;; Front/back stream length differential factor.
(: stream-length-factor fixnum)
(define stream-length-factor 3)

(: make-deque (fixnum * fixnum * -> ideque))
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

;;;; Basic operations

(: ideque-empty? (ideque -> boolean))
(define (ideque-empty? dq)
  (assert-type 'ideque-empty? (ideque? dq))
  (and (zero? (dq-lenf dq))
       (zero? (dq-lenr dq))))

(: ideque-add-front (ideque * -> ideque))
(define (ideque-add-front dq x)
  (assert-type 'ideque-add-front (ideque? dq))
  (make-deque (+ (dq-lenf dq) 1)
              (stream-cons x (dq-f dq))
              (dq-lenr dq)
              (dq-r dq)))

(: ideque-front (ideque -> *))
(define (ideque-front dq)
  (assert-type 'ideque-front (ideque? dq))
  (if (zero? (dq-lenf dq))
      (if (zero? (dq-lenr dq))
          (error "Empty deque:" dq)
          (stream-car (dq-r dq)))
      (stream-car (dq-f dq))))

(: ideque-remove-front (ideque -> ideque))
(define (ideque-remove-front dq)
  (assert-type 'ideque-remove-front (ideque? dq))
  (if (zero? (dq-lenf dq))
      (if (zero? (dq-lenr dq))
          (error "Empty deque:" dq)
          *empty*)
      (make-deque (- (dq-lenf dq) 1)
                  (stream-cdr (dq-f dq))
                  (dq-lenr dq)
                  (dq-r dq))))

(: ideque-add-back (ideque * -> ideque))
(define (ideque-add-back dq x)
  (assert-type 'ideque-add-back (ideque? dq))
  (make-deque (dq-lenf dq)
              (dq-f dq)
              (+ (dq-lenr dq) 1)
              (stream-cons x (dq-r dq))))

(: ideque-back (ideque -> *))
(define (ideque-back dq)
  (assert-type 'ideque-back (ideque? dq))
  (if (zero? (dq-lenr dq))
      (if (zero? (dq-lenf dq))
          (error "Empty deque:" dq)
          (stream-car (dq-f dq)))
      (stream-car (dq-r dq))))

(: ideque-remove-back (ideque -> ideque))
(define (ideque-remove-back dq)
  (assert-type 'ideque-remove-back (ideque? dq))
  (if (zero? (dq-lenr dq))
      (if (zero? (dq-lenf dq))
          (error "Empty deque:" dq)
          *empty*)
      (make-deque (dq-lenf dq)
                  (dq-f dq)
                  (- (dq-lenr dq) 1)
                  (stream-cdr (dq-r dq)))))

(: ideque-reverse (ideque -> ideque))
(define (ideque-reverse dq)
  (assert-type 'ideque-reverse (ideque? dq))
  (if (ideque-empty? dq)
      *empty*
      (%make-dq (dq-lenr dq) (dq-r dq) (dq-lenf dq) (dq-f dq))))

;;; Exported constructors

(: ideque (#!rest -> ideque))
(define (ideque . args)
  (if (null? args)
      *empty*
      (list->ideque args)))

(: ideque-tabulate (fixnum procedure -> ideque))
(define (ideque-tabulate size init)
  (let ((lenf (quotient size 2))
        (lenr (quotient (+ size 1) 2)))
    (%make-dq lenf
              (stream-unfold init
                             (lambda (n) (< n lenf))
                             (lambda (n) (+ n 1))
                             0)
              lenr
              (stream-unfold (lambda (n) (init (- size n 1)))
                             (lambda (n) (< n lenr))
                             (lambda (n) (+ n 1))
                             0))))

(: ideque-unfold (procedure procedure procedure * -> ideque))
(define (ideque-unfold p f g seed)
  (list->ideque (unfold p f g seed)))

(: ideque-unfold-right (procedure procedure procedure * -> ideque))
(define (ideque-unfold-right p f g seed)
  (ideque-reverse (list->ideque (unfold p f g seed))))

;;;; Other operations

(: ideque= (procedure #!rest ideque -> boolean))
(define ideque=
  (case-lambda
    ((elt=) #t)
    ((elt= dq)
     (assert-type 'ideque= (ideque? dq))
     #t)
    ((elt= dq1 dq2) (%ideque=-binary elt= dq1 dq2))
    ((elt= . dqs)
     ;; The comparison scheme is the same as srfi-1's list=.
     (apply list= elt= (map ideque->list dqs)))))

(: %ideque-same-length (ideque ideque -> boolean))
(define (%ideque-same-length dq1 dq2)
  (= (ideque-length dq1) (ideque-length dq2)))

;; we optimize two-arg case
(: %ideque=-binary (procedure ideque ideque -> boolean))
(define (%ideque=-binary elt= dq1 dq2)
  (assert-type 'ideque= (ideque? dq1))
  (assert-type 'ideque= (ideque? dq2))
  (or (eq? dq1 dq2)
      (and (%ideque-same-length dq1 dq2)
           (receive (x t1 t2)
                    (stream-prefix= elt= (dq-f dq1) (dq-f dq2))
             (and x
                  (receive (y r1 r2)
                           (stream-prefix= elt= (dq-r dq1) (dq-r dq2))
                    (and y
                         (if (null? t1)
                             (stream=? elt= t2 (stream-reverse r1))
                             (stream=? elt= t1 (stream-reverse r2))))))))))


(: ideque-ref (ideque fixnum -> *))
(define (ideque-ref dq n)
  (assert-type 'ideque-ref (ideque? dq))
  (let ((len (+ (dq-lenf dq) (dq-lenr dq))))
    (cond ((or (< n 0) (>= n len)) (error "Index out of range:" n))
          ((< n (dq-lenf dq)) (stream-ref (dq-f dq) n))
          (else (stream-ref (dq-r dq) (- len n 1))))))

(: %ideque-take (ideque fixnum -> *))
(define (%ideque-take dq n)             ; n is within the range
  (let ((lenf (dq-lenf dq))
        (f    (dq-f dq))
        (lenr (dq-lenr dq)))
    (if (<= n lenf)
        (make-deque n (stream-take n f) 0 stream-null)
        (let ((lenr. (- lenr (- n lenf))))
          (make-deque lenf f lenr. (stream-drop lenr. (dq-r dq)))))))

(: %ideque-drop (ideque fixnum -> *))
(define (%ideque-drop dq n)             ; n is within the range
  (let ((lenf (dq-lenf dq))
        (f    (dq-f dq))
        (lenr (dq-lenr dq))
        (r    (dq-r dq)))
    (if (<= n lenf)
        (make-deque (- lenf n) (stream-drop n f) lenr r)
        (let ((lenr. (- lenr (- n lenf))))
          (make-deque 0 stream-null lenr. (stream-take lenr. r))))))

(: %check-length (symbol ideque fixnum -> undefined))
(define (%check-length loc dq n)
  (unless (<= 0 n (ideque-length dq))
    (bounds-exception loc "argument is out of range" n dq)))

(: ideque-take (ideque fixnum -> ideque))
(define (ideque-take dq n)
  (assert-type 'ideque-take (ideque? dq))
  (%check-length 'ideque-take dq n)
  (%ideque-take dq n))

(: ideque-take-right (ideque fixnum -> ideque))
(define (ideque-take-right dq n)
  (assert-type 'ideque-take-right (ideque? dq))
  (%check-length 'ideque-take-right dq n)
  (%ideque-drop dq (- (ideque-length dq) n)))

(: ideque-drop (ideque fixnum -> ideque))
(define (ideque-drop dq n)
  (assert-type 'ideque-drop (ideque? dq))
  (%check-length 'ideque-drop dq n)
  (%ideque-drop dq n))

(: ideque-drop-right (ideque fixnum -> ideque))
(define (ideque-drop-right dq n)
  (assert-type 'ideque-drop-right (ideque? dq))
  (%check-length 'ideque-drop-right dq n)
  (%ideque-take dq (- (ideque-length dq) n)))

(: ideque-split-at (ideque fixnum -> ideque ideque))
(define (ideque-split-at dq n)
  (assert-type 'ideque-split-at (ideque? dq))
  (%check-length 'ideque-split-at dq n)
  (values (%ideque-take dq n)
          (%ideque-drop dq n)))

(: ideque-length (ideque -> fixnum))
(define (ideque-length dq)
  (assert-type 'ideque-length (ideque? dq))
  (+ (dq-lenf dq) (dq-lenr dq)))

(: ideque-append (#!rest ideque -> ideque))
(define (ideque-append . dqs)
  ;; We could save some list copying by carefully split dqs into front and
  ;; rear groups and append separately, but for now we don't bother...
  (list->ideque (concatenate (map ideque->list dqs))))

(: ideque-count (procedure ideque -> fixnum))
(define (ideque-count pred dq)
  (assert-type 'ideque-count (ideque? dq))
  (+ (stream-count pred (dq-f dq)) (stream-count pred (dq-r dq))))

(: ideque-zip (ideque #!rest ideque -> ideque))
(define (ideque-zip dq . dqs)
  ;; An easy way.
  (let ((elts (apply zip (ideque->list dq) (map ideque->list dqs))))
    (make-deque (length elts) (list->stream elts) 0 stream-null)))

(: ideque-map (procedure ideque -> ideque))
(define (ideque-map proc dq)
  (assert-type 'ideque-map (ideque? dq))
  (%make-dq (dq-lenf dq) (stream-map proc (dq-f dq))
            (dq-lenr dq) (stream-map proc (dq-r dq))))

(: ideque-filter-map (procedure ideque -> ideque))
(define (ideque-filter-map proc dq)
  (assert-type 'ideque-filter-map (ideque? dq))
  (let ((f (stream-filter-map proc (dq-f dq)))
        (r (stream-filter-map proc (dq-r dq))))
    (make-deque (stream-length f) f (stream-length r) r)))

(: ideque-for-each (procedure ideque -> undefined))
(define (ideque-for-each proc dq)
  (assert-type 'ideque-for-each (ideque? dq))
  (stream-for-each proc (dq-f dq))
  (stream-for-each proc (stream-reverse (dq-r dq))))

(: ideque-for-each-right (procedure ideque -> undefined))
(define (ideque-for-each-right proc dq)
  (assert-type 'ideque-for-each-right (ideque? dq))
  (stream-for-each proc (dq-r dq))
  (stream-for-each proc (stream-reverse (dq-f dq))))

(: ideque-fold (procedure * ideque -> *))
(define (ideque-fold proc knil dq)
  (let ((proc* (lambda (acc x) (proc x acc))))  ; stream-fold compat
    (assert-type 'ideque-fold (ideque? dq))
    (stream-fold proc*
                 (stream-fold proc* knil (dq-f dq))
                 (stream-reverse (dq-r dq)))))

;; There's no stream-fold-right, so just convert dq.
(: ideque-fold-right (procedure * ideque -> *))
(define (ideque-fold-right proc knil dq)
  (assert-type 'ideque-fold-right (ideque? dq))
  (fold-right proc knil (ideque->list dq)))

(: ideque-append-map (procedure ideque -> ideque))
(define (ideque-append-map proc dq)
  ;; can be cleverer, but for now...
  (list->ideque (append-map proc (ideque->list dq))))

(: %ideque-filter (procedure ideque -> ideque))
(define (%ideque-filter pred dq)
  (assert-type '%ideque-filter (ideque? dq))
  (let ((f (stream-filter pred (dq-f dq)))
        (r (stream-filter pred (dq-r dq))))
    (make-deque (stream-length f) f (stream-length r) r)))

(: ideque-filter (procedure ideque -> ideque))
(define (ideque-filter pred dq) (%ideque-filter pred dq))
(: ideque-remove (procedure ideque -> ideque))
(define (ideque-remove pred dq)
  (%ideque-filter (lambda (x) (not (pred x))) dq))

(: ideque-partition (procedure ideque -> ideque ideque))
(define (ideque-partition pred dq)
  (assert-type 'ideque-partition (ideque? dq))
  (receive (f1 f2) (stream-partition pred (dq-f dq))
    (receive (r1 r2) (stream-partition pred (dq-r dq))
      (values (make-deque (stream-length f1) f1 (stream-length r1) r1)
              (make-deque (stream-length f2) f2 (stream-length r2) r2)))))

(: *not-found* (pair boolean boolean))
(define-constant *not-found* (cons #f #f)) ; unique value

(: %search (procedure * * procedure -> *))
(define (%search pred seq1 seq2 failure)
  ;; We could write seek as CPS, but we employ *not-found* instead to avoid
  ;; closure allocation.
  (define (seek pred s)
    (cond ((stream-null? s) *not-found*)
          ((pred (stream-car s)) (stream-car s))
          (else (seek pred (stream-cdr s)))))
  (let ((r (seek pred seq1)))
    (if (not (eq? r *not-found*))
        r
        (let ((r (seek pred (stream-reverse seq2))))
          (if (not (eq? r *not-found*))
              r
              (failure))))))

(: ideque-find (procedure ideque #!optional procedure -> *))
(define (ideque-find pred dq . opts)
  (assert-type 'ideque-find (ideque? dq))
  (let ((failure (if (pair? opts) (car opts) (lambda () #f))))
    (%search pred (dq-f dq) (dq-r dq) failure)))

(: ideque-find-right (procedure ideque #!optional procedure -> *))
(define (ideque-find-right pred dq . opts)
  (assert-type 'ideque-find-right (ideque? dq))
  (let ((failure (if (pair? opts) (car opts) (lambda () #f))))
    (%search pred (dq-r dq) (dq-f dq) failure)))

(: ideque-take-while (procedure ideque -> ideque))
(define (ideque-take-while pred dq)
  (assert-type 'ideque-take-while (ideque? dq))
  (receive (hd tl) (stream-span pred (dq-f dq))
    (if (stream-null? tl)
        (receive (hd. tl.) (stream-span pred (stream-reverse (dq-r dq)))
          (make-deque (dq-lenf dq)
                      (dq-f dq)
                      (stream-length hd.)
                      (stream-reverse hd.)))
        (make-deque (stream-length hd) hd 0 stream-null))))

(: ideque-take-while-right (procedure ideque -> ideque))
(define (ideque-take-while-right pred dq)
  (assert-type 'ideque-take-while-right (ideque? dq))
  (ideque-reverse (ideque-take-while pred (ideque-reverse dq))))

(: ideque-drop-while (procedure ideque -> ideque))
(define (ideque-drop-while pred dq)
  (assert-type 'ideque-drop-while (ideque? dq))
  (receive (hd tl) (stream-span pred (dq-f dq))
    (if (stream-null? tl)
        (receive (hd. tl.) (stream-span pred (stream-reverse (dq-r dq)))
          (make-deque (stream-length tl.) tl. 0 stream-null))
        (make-deque (stream-length tl) tl (dq-lenr dq) (dq-r dq)))))

(: ideque-drop-while-right (procedure ideque -> ideque))
(define (ideque-drop-while-right pred dq)
  (assert-type 'ideque-drop-while-right (ideque? dq))
  (ideque-reverse (ideque-drop-while pred (ideque-reverse dq))))

(: %idq-span-break (procedure procedure ideque -> ideque ideque))
(define (%idq-span-break op pred dq)
  (assert-type '%idq-span-break (ideque? dq))
  (receive (head tail) (op pred (dq-f dq))
    (if (null? tail)
        (receive (head. tail.) (op pred (stream-reverse (dq-r dq)))
          (values (make-deque (stream-length head)
                              head
                              (stream-length head.)
                              (stream-reverse head.))
                  (make-deque (stream-length tail.) tail. 0 stream-null)))
        (values
         (make-deque (stream-length head) head 0 stream-null)
         (make-deque (stream-length tail) tail (dq-lenr dq) (dq-r dq))))))

(: ideque-span (procedure ideque -> ideque ideque))
(define (ideque-span pred dq) (%idq-span-break stream-span pred dq))
(: ideque-break (procedure ideque -> ideque ideque))
(define (ideque-break pred dq) (%idq-span-break stream-break pred dq))

(: ideque-any (procedure ideque -> *))
(define (ideque-any pred dq)
  (assert-type 'ideque-any (ideque? dq))
  (if (stream-null? (dq-r dq))
      (stream-any pred (dq-f dq))
      (or (stream-any pred (dq-f dq))
          (stream-any pred (stream-reverse (dq-r dq))))))

(: ideque-every (procedure ideque -> *))
(define (ideque-every pred dq)
  (assert-type 'ideque-every (ideque? dq))
  (if (stream-null? (dq-r dq))
      (stream-every pred (dq-f dq))
      (and (stream-every pred (dq-f dq))
           (stream-every pred (stream-reverse (dq-r dq))))))

(: ideque->list (ideque -> list))
(define (ideque->list dq)
  (assert-type 'ideque->list (ideque? dq))
  (append (stream->list (dq-f dq))
          (stream->list (stream-reverse (dq-r dq)))))

(: list->ideque (list -> ideque))
(define (list->ideque lis)
  (make-deque (length lis) (list->stream lis) 0 stream-null))

(: ideque->generator (ideque -> procedure))
(define (ideque->generator dq)
  (assert-type 'ideque->generator (ideque? dq))
  (lambda ()
    (if (ideque-empty? dq)
        (eof-object)
        (let ((v (ideque-front dq)))
          (set! dq (ideque-remove-front dq))
          v))))

(: generator->ideque (procedure -> ideque))
(define (generator->ideque gen)
  (list->ideque (generator->list gen)))
