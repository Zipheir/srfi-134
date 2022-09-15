;;;; See LICENSE for copyright information.

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

;;;; Utility

(: natural-fixnum? (* --> boolean))
(define (natural-fixnum? x)
  (and (fixnum? x) (not (negative? x))))

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
          (bounds-exception 'ideque-front "empty deque" dq)
          (stream-car (dq-r dq)))
      (stream-car (dq-f dq))))

(: ideque-remove-front (ideque -> ideque))
(define (ideque-remove-front dq)
  (assert-type 'ideque-remove-front (ideque? dq))
  (if (zero? (dq-lenf dq))
      (if (zero? (dq-lenr dq))
          (bounds-exception 'ideque-remove-front "empty deque" dq)
          the-empty-ideque)
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
          (stream-car (dq-f dq)))
      (stream-car (dq-r dq))))

(: ideque-remove-back (ideque -> ideque))
(define (ideque-remove-back dq)
  (assert-type 'ideque-remove-back (ideque? dq))
  (if (zero? (dq-lenr dq))
      (if (zero? (dq-lenf dq))
          (bounds-exception 'ideque-remove-back "empty deque" dq)
          the-empty-ideque)
      (make-deque (dq-lenf dq)
                  (dq-f dq)
                  (- (dq-lenr dq) 1)
                  (stream-cdr (dq-r dq)))))

(: ideque-reverse (ideque -> ideque))
(define (ideque-reverse dq)
  (assert-type 'ideque-reverse (ideque? dq))
  (if (ideque-empty? dq)
      the-empty-ideque
      (make-deque (dq-lenr dq) (dq-r dq) (dq-lenf dq) (dq-f dq))))

;;; Exported constructors

(: ideque (#!rest -> ideque))
(define (ideque . args)
  (if (null? args)
      the-empty-ideque
      (list->ideque args)))

(: ideque-tabulate (fixnum procedure -> ideque))
(define (ideque-tabulate size init)
  (assert-type 'ideque-tabulate (natural-fixnum? size))
  (assert-type 'ideque-tabulate (procedure? init))
  (let ((lenf (quotient size 2))
        (lenr (quotient (+ size 1) 2)))
    (make-deque lenf
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
  (assert-type 'ideque-unfold (procedure? p))
  (assert-type 'ideque-unfold (procedure? f))
  (assert-type 'ideque-unfold (procedure? g))
  (list->ideque (unfold p f g seed)))

(: ideque-unfold-right (procedure procedure procedure * -> ideque))
(define (ideque-unfold-right p f g seed)
  (assert-type 'ideque-unfold-right (procedure? p))
  (assert-type 'ideque-unfold-right (procedure? f))
  (assert-type 'ideque-unfold-right (procedure? g))
  (ideque-reverse (list->ideque (unfold p f g seed))))

;;;; Other operations

(: ideque= (procedure #!rest ideque -> boolean))
(define ideque=
  (case-lambda
    ((elt=)
     (assert-type 'ideque= (procedure? elt=))
     #t)
    ((elt= dq)
     (assert-type 'ideque= (procedure? elt=))
     (assert-type 'ideque= (ideque? dq))
     #t)
    ((elt= dq1 dq2) (%ideque=-binary elt= dq1 dq2))
    ((elt= . dqs)
     (assert-type 'ideque= (procedure? elt=))
     (assert-type 'ideque= (every ideque? dqs))
     ;; The comparison scheme is the same as srfi-1's list=.
     (apply list= elt= (map ideque->list dqs)))))

(: %ideque-same-length (ideque ideque -> boolean))
(define (%ideque-same-length dq1 dq2)
  (= (%ideque-length dq1) (%ideque-length dq2)))

;; we optimize two-arg case
(: %ideque=-binary (procedure ideque ideque -> boolean))
(define (%ideque=-binary elt= dq1 dq2)
  (assert-type 'ideque= (procedure? elt=))
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
  (assert-type 'ideque-ref (natural-fixnum? n))
  (let ((len (+ (dq-lenf dq) (dq-lenr dq))))
    (cond ((or (< n 0) (>= n len))
           (bounds-exception 'ideque-ref
                             "argument is out of range"
                             n
                             dq))
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
  (unless (<= 0 n (%ideque-length dq))
    (bounds-exception loc "argument is out of range" n dq)))

(: ideque-take (ideque fixnum -> ideque))
(define (ideque-take dq n)
  (assert-type 'ideque-take (ideque? dq))
  (assert-type 'ideque-take (natural-fixnum? n))
  (%check-length 'ideque-take dq n)
  (%ideque-take dq n))

(: ideque-take-right (ideque fixnum -> ideque))
(define (ideque-take-right dq n)
  (assert-type 'ideque-take-right (ideque? dq))
  (assert-type 'ideque-take-right (natural-fixnum? n))
  (%check-length 'ideque-take-right dq n)
  (%ideque-drop dq (- (%ideque-length dq) n)))

(: ideque-drop (ideque fixnum -> ideque))
(define (ideque-drop dq n)
  (assert-type 'ideque-drop (ideque? dq))
  (assert-type 'ideque-drop (natural-fixnum? n))
  (%check-length 'ideque-drop dq n)
  (%ideque-drop dq n))

(: ideque-drop-right (ideque fixnum -> ideque))
(define (ideque-drop-right dq n)
  (assert-type 'ideque-drop-right (ideque? dq))
  (assert-type 'ideque-drop-right (natural-fixnum? n))
  (%check-length 'ideque-drop-right dq n)
  (%ideque-take dq (- (%ideque-length dq) n)))

(: ideque-split-at (ideque fixnum -> ideque ideque))
(define (ideque-split-at dq n)
  (assert-type 'ideque-split-at (ideque? dq))
  (assert-type 'ideque-split-at (natural-fixnum? n))
  (%check-length 'ideque-split-at dq n)
  (values (%ideque-take dq n)
          (%ideque-drop dq n)))

(: ideque-length (ideque -> fixnum))
(define (ideque-length dq)
  (assert-type 'ideque-length (ideque? dq))
  (%ideque-length dq))

;; Version with no type-check for internal use.
(: %ideque-length (ideque -> fixnum))
(define (%ideque-length dq)
  (+ (dq-lenf dq) (dq-lenr dq)))

(: ideque-append (#!rest ideque -> ideque))
(define ideque-append
  (case-lambda
    ((dq)
     (assert-type 'ideque-append (ideque? dq))
     dq)
    ((dq1 dq2)  ; fast path
     (assert-type 'ideque-append (ideque? dq1))
     (assert-type 'ideque-append (ideque? dq2))
     (%ideque-append-binary dq1 dq2))
    (dqs
     (assert-type 'ideque-append (every ideque? dqs))
     (list->ideque (concatenate (map ideque->list dqs))))))

(: %ideque-append-binary (ideque ideque -> ideque))
(define (%ideque-append-binary dq1 dq2)
  (cond ((zero? (%ideque-length dq1)) dq2)
        ((zero? (%ideque-length dq2)) dq1)
        (else
         (make-deque (%ideque-length dq1)
                     (stream-append (dq-f dq1)
                                    (stream-reverse (dq-r dq1)))
                     (%ideque-length dq2)
                     (stream-append (dq-r dq2)
                                    (stream-reverse (dq-f dq2)))))))

(: ideque-count (procedure ideque -> fixnum))
(define (ideque-count pred dq)
  (assert-type 'ideque-count (procedure? pred))
  (assert-type 'ideque-count (ideque? dq))
  (+ (stream-count pred (dq-f dq)) (stream-count pred (dq-r dq))))

(: ideque-zip (ideque #!rest ideque -> ideque))
(define (ideque-zip dq . dqs)
  (assert-type 'ideque-zip (ideque? dq))
  (assert-type 'ideque-zip (every ideque? dqs))
  ;; An easy way.
  (let ((elts (apply zip (ideque->list dq) (map ideque->list dqs))))
    (make-deque (length elts) (list->stream elts) 0 stream-null)))

(: ideque-map (procedure ideque -> ideque))
(define (ideque-map proc dq)
  (assert-type 'ideque-map (procedure? proc))
  (assert-type 'ideque-map (ideque? dq))
  (make-deque (dq-lenf dq) (stream-map proc (dq-f dq))
            (dq-lenr dq) (stream-map proc (dq-r dq))))

(: ideque-filter-map (procedure ideque -> ideque))
(define (ideque-filter-map proc dq)
  (assert-type 'ideque-filter-map (procedure? proc))
  (assert-type 'ideque-filter-map (ideque? dq))
  (let ((f (stream-filter-map proc (dq-f dq)))
        (r (stream-filter-map proc (dq-r dq))))
    (make-deque (stream-length f) f (stream-length r) r)))

(: ideque-for-each (procedure ideque -> undefined))
(define (ideque-for-each proc dq)
  (assert-type 'ideque-for-each (procedure? proc))
  (assert-type 'ideque-for-each (ideque? dq))
  (stream-for-each proc (dq-f dq))
  (stream-for-each proc (stream-reverse (dq-r dq))))

(: ideque-for-each-right (procedure ideque -> undefined))
(define (ideque-for-each-right proc dq)
  (assert-type 'ideque-for-each-right (procedure? proc))
  (assert-type 'ideque-for-each-right (ideque? dq))
  (stream-for-each proc (dq-r dq))
  (stream-for-each proc (stream-reverse (dq-f dq))))

(: ideque-fold (procedure * ideque -> *))
(define (ideque-fold proc knil dq)
  (assert-type 'ideque-fold (procedure? proc))
  (let ((proc* (lambda (acc x) (proc x acc))))  ; stream-fold compat
    (assert-type 'ideque-fold (ideque? dq))
    (stream-fold proc*
                 (stream-fold proc* knil (dq-f dq))
                 (stream-reverse (dq-r dq)))))

;; There's no stream-fold-right, so just convert dq.
(: ideque-fold-right (procedure * ideque -> *))
(define (ideque-fold-right proc knil dq)
  (assert-type 'ideque-fold-right (procedure? proc))
  (assert-type 'ideque-fold-right (ideque? dq))
  (fold-right proc knil (ideque->list dq)))

(: ideque-append-map (procedure ideque -> ideque))
(define (ideque-append-map proc dq)
  (assert-type 'ideque-append-map (procedure? proc))
  (assert-type 'ideque-append-map (ideque? dq))
  ;; can be cleverer, but for now...
  (list->ideque (append-map proc (ideque->list dq))))

(: %ideque-filter (symbol procedure ideque -> ideque))
(define (%ideque-filter loc pred dq)
  (assert-type loc (procedure? pred))
  (assert-type loc (ideque? dq))
  (let ((f (stream-filter pred (dq-f dq)))
        (r (stream-filter pred (dq-r dq))))
    (make-deque (stream-length f) f (stream-length r) r)))

(: ideque-filter (procedure ideque -> ideque))
(define (ideque-filter pred dq) (%ideque-filter 'ideque-filter pred dq))

(: ideque-remove (procedure ideque -> ideque))
(define (ideque-remove pred dq)
  (%ideque-filter 'ideque-remove (lambda (x) (not (pred x))) dq))

(: ideque-partition (procedure ideque -> ideque ideque))
(define (ideque-partition pred dq)
  (assert-type 'ideque-partition (procedure? pred))
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
  (assert-type 'ideque-find (procedure? pred))
  (assert-type 'ideque-find (ideque? dq))
  (unless (<= (length opts) 1)
    (arity-exception 'ideque-find opts))
  (let ((failure (if (pair? opts) (car opts) (lambda () #f))))
    (assert-type 'ideque-find (procedure? failure))
    (%search pred (dq-f dq) (dq-r dq) failure)))

(: ideque-find-right (procedure ideque #!optional procedure -> *))
(define (ideque-find-right pred dq . opts)
  (assert-type 'ideque-find-right (procedure? pred))
  (assert-type 'ideque-find-right (ideque? dq))
  (let ((failure (if (pair? opts) (car opts) (lambda () #f))))
    (assert-type 'ideque-find (procedure? failure))
    (%search pred (dq-r dq) (dq-f dq) failure)))

(: ideque-take-while (procedure ideque -> ideque))
(define (ideque-take-while pred dq)
  (assert-type 'ideque-take-while (procedure? pred))
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
  (assert-type 'ideque-take-while-right (procedure? pred))
  (assert-type 'ideque-take-while-right (ideque? dq))
  (ideque-reverse (ideque-take-while pred (ideque-reverse dq))))

(: ideque-drop-while (procedure ideque -> ideque))
(define (ideque-drop-while pred dq)
  (assert-type 'ideque-drop-while (procedure? pred))
  (assert-type 'ideque-drop-while (ideque? dq))
  (receive (hd tl) (stream-span pred (dq-f dq))
    (if (stream-null? tl)
        (receive (hd. tl.) (stream-span pred (stream-reverse (dq-r dq)))
          (make-deque (stream-length tl.) tl. 0 stream-null))
        (make-deque (stream-length tl) tl (dq-lenr dq) (dq-r dq)))))

(: ideque-drop-while-right (procedure ideque -> ideque))
(define (ideque-drop-while-right pred dq)
  (assert-type 'ideque-drop-while-right (procedure? pred))
  (assert-type 'ideque-drop-while-right (ideque? dq))
  (ideque-reverse (ideque-drop-while pred (ideque-reverse dq))))

(: %idq-span-break (symbol procedure procedure ideque -> ideque ideque))
(define (%idq-span-break loc op pred dq)
  (assert-type loc (ideque? dq))
  (assert-type loc (procedure? pred))
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
(define (ideque-span pred dq)
  (%idq-span-break 'ideque-span stream-span pred dq))

(: ideque-break (procedure ideque -> ideque ideque))
(define (ideque-break pred dq)
  (%idq-span-break 'ideque-break stream-break pred dq))

(: ideque-any (procedure ideque -> *))
(define (ideque-any pred dq)
  (assert-type 'ideque-any (procedure? pred))
  (assert-type 'ideque-any (ideque? dq))
  (if (stream-null? (dq-r dq))
      (stream-any pred (dq-f dq))
      (or (stream-any pred (dq-f dq))
          (stream-any pred (stream-reverse (dq-r dq))))))

(: ideque-every (procedure ideque -> *))
(define (ideque-every pred dq)
  (assert-type 'ideque-every (procedure? pred))
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
  (assert-type 'list->ideque (or (pair? lis) (null? lis)))
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
  (assert-type 'generator->ideque (procedure? gen))
  (list->ideque (generator->list gen)))
