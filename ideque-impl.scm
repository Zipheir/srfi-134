;;;
;;; srfi-134 reference implementation
;;;
;;;   Copyright (c) 2015  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; This is similar to the banker's deque as described in
;; Chris Okasaki's Purely Functional Data Structures, but
;; does not use lazy evaluation.  Thus, it probably does *not*
;; provide amortized O(1) running time in all of the
;; situations in which the SRFI requires it.  Still, it's
;; fast enough for most purposes. --Zipheir

;; Originally written for Gauche, and ported to R7RS.

;; Requires srfi-1, srfi-9, srfi-121.

;;; Utility

(define (exact-natural? x)
  (and (exact-integer? x) (not (negative? x))))

;;;
;;; Record
;;;

(define-record-type <ideque> (%make-dq lenf f lenr r) ideque?
  (lenf dq-lenf : integer)   ; length of front chain
  (f    dq-f    : list)     ; front chain
  (lenr dq-lenr : integer)   ; length of rear chain
  (r    dq-r    : list))    ; rear chain

(define-type ideque (struct <ideque>))

;; We use a singleton for empty deque
(define *empty* (%make-dq 0 '() 0 '()))

;;;
;;; Constructors
;;;

;; API
(: ideque (#!rest --> ideque))
(define (ideque . args) (list->ideque args))

;; API
(: ideque-tabulate (integer * --> ideque))
(define (ideque-tabulate size init)
  (assert-type 'ideque-tabulate (exact-natural? size))
  (assert-type 'ideque-tabulate (procedure? init))
  (let ((lenf (quotient size 2))
        (lenr (quotient (+ size 1) 2)))
    (%make-dq lenf (list-tabulate lenf init)
              lenr (unfold (lambda (n) (= n lenr))
                           (lambda (n) (init (- size n 1)))
                           (lambda (n) (+ n 1))
                           0))))

;; API
(: ideque-unfold (procedure procedure procedure * --> ideque))
(define (ideque-unfold p f g seed)
  (assert-type 'ideque-unfold (procedure? p))
  (assert-type 'ideque-unfold (procedure? f))
  (assert-type 'ideque-unfold (procedure? g))
  (list->ideque (unfold p f g seed)))

;; API
(: ideque-unfold-right (procedure procedure procedure * --> ideque))
(define (ideque-unfold-right p f g seed)
  (assert-type 'ideque-unfold (procedure? p))
  (assert-type 'ideque-unfold (procedure? f))
  (assert-type 'ideque-unfold (procedure? g))
  (list->ideque (unfold-right p f g seed)))
;; alternatively:
;; (ideque-reverse (list->ideque (unfold p f g seed)))

;; Internal constructor.  Returns a new ideque, with balancing 'front' and
;; 'rear' chains.  (The name 'check' comes from Okasaki's book.)

(define C 3)

(: check (integer list integer list --> ideque))
(define (check lenf f lenr r)
  (cond ((> lenf (+ (* lenr C) 1))
         (let* ((i (quotient (+ lenf lenr) 2))
                (j (- (+ lenf lenr) i))
                (f. (take f i))
                (r. (append r (reverse (drop f i)))))
           (%make-dq i f. j r.)))
        ((> lenr (+ (* lenf C) 1))
         (let* ((j (quotient (+ lenf lenr) 2))
                (i (- (+ lenf lenr) j))
                (r. (take r j))
                (f. (append f (reverse (drop r j)))))
           (%make-dq i f. j r.)))
        (else (%make-dq lenf f lenr r))))

;;;
;;; Basic operations
;;;

;; API
(: ideque-empty? (ideque --> boolean))
(define (ideque-empty? dq)
  (assert-type 'ideque-empty? (ideque? dq))
  (and (zero? (dq-lenf dq))
       (zero? (dq-lenr dq))))

;; API
(: ideque-add-front (ideque * --> ideque))
(define (ideque-add-front dq x)
  (assert-type 'ideque-add-front (ideque? dq))
  (check (+ (dq-lenf dq) 1) (cons x (dq-f dq)) (dq-lenr dq) (dq-r dq)))

;; API
(: ideque-front (ideque --> *))
(define (ideque-front dq)
  (assert-type 'ideque-front (ideque? dq))
  (if (zero? (dq-lenf dq))
    (if (zero? (dq-lenr dq))
      (error 'ideque-front "empty deque" dq)
      (car (dq-r dq)))
    (car (dq-f dq))))

;; API
(: ideque-remove-front (ideque --> ideque))
(define (ideque-remove-front dq)
  (assert-type 'ideque-remove-front (ideque? dq))
  (if (zero? (dq-lenf dq))
    (if (zero? (dq-lenr dq))
      (error 'ideque-remove-front "empty deque" dq)
      *empty*)
    (check (- (dq-lenf dq) 1) (cdr (dq-f dq)) (dq-lenr dq) (dq-r dq))))

;; API
(: ideque-add-back (ideque * --> ideque))
(define (ideque-add-back dq x)
  (assert-type 'ideque-add-back (ideque? dq))
  (check (dq-lenf dq) (dq-f dq) (+ (dq-lenr dq) 1) (cons x (dq-r dq))))

;; API
(: ideque-back (ideque --> *))
(define (ideque-back dq)
  (assert-type 'ideque-back (ideque? dq))
  (if (zero? (dq-lenr dq))
    (if (zero? (dq-lenf dq))
      (error 'ideque-back "empty deque" dq)
      (car (dq-f dq)))
    (car (dq-r dq))))

;; API
(: ideque-remove-back (ideque --> ideque))
(define (ideque-remove-back dq)
  (assert-type 'ideque-remove-back (ideque? dq))
  (if (zero? (dq-lenr dq))
    (if (zero? (dq-lenf dq))
      (error 'ideque-remove-back "empty deque" dq)
      *empty*)
    (check (dq-lenf dq) (dq-f dq) (- (dq-lenr dq) 1) (cdr (dq-r dq)))))

;; API
(: ideque-reverse (ideque --> ideque))
(define (ideque-reverse dq)
  (assert-type 'ideque-reverse (ideque? dq))
  (if (ideque-empty? dq)
    *empty*
    (%make-dq (dq-lenr dq) (dq-r dq) (dq-lenf dq) (dq-f dq))))

;;
;; Other operations
;;

;; API
(: ideque= (procedure #!rest ideque --> boolean))
(define ideque=
  (case-lambda
    ((elt=)
     (assert-type 'ideque= (procedure? elt=))
     #t)
    ((elt= ideque)
     (assert-type 'ideque= (procedure? elt=))
     (assert-type 'ideque= (ideque? ideque))
     #t)
    ((elt= dq1 dq2)
     ;; we optimize two-arg case
     (assert-type 'ideque= (procedure? elt=))
     (assert-type 'ideque= (ideque? dq1))
     (assert-type 'ideque= (ideque? dq2))
     (or (eq? dq1 dq2)
         (let ((len1 (+ (dq-lenf dq1) (dq-lenr dq1)))
               (len2 (+ (dq-lenf dq2) (dq-lenr dq2))))
           (and (= len1 len2)
                (receive (x t1 t2) (list-prefix= elt= (dq-f dq1) (dq-f dq2))
                  (and x
                       (receive (y r1 r2) (list-prefix= elt= (dq-r dq1) (dq-r dq2))
                         (and y
                              (if (null? t1)
                                (list= elt= t2 (reverse r1))
                                (list= elt= t1 (reverse r2)))))))))))
    ((elt= . dqs)
     (assert-type 'ideque= (procedure? elt=))
     (assert-type 'ideque= (every ideque? dqs))
     ;; The comparison scheme is the same as srfi-1's list=.
     (apply list= elt= (map ideque->list dqs)))))

;; Compare two lists up to whichever shorter one.
;; Returns the compare result and the tails of uncompared lists.
(: list-prefix= ((* * -> boolean) list list --> boolean list list))
(define (list-prefix= elt= a b)
  (let loop ((a a) (b b))
    (cond ((or (null? a) (null? b)) (values #t a b))
          ((elt= (car a) (car b)) (loop (cdr a) (cdr b)))
          (else (values #f a b)))))

;; API
(: ideque-ref (ideque integer --> *))
(define (ideque-ref dq n)
  (assert-type 'ideque-ref (ideque? dq))
  (assert-type 'ideque-ref (exact-integer? n))
  (let ((len (+ (dq-lenf dq) (dq-lenr dq))))
    (cond ((or (< n 0) (>= n len))
           (bounds-exception 'ideque-ref "index out of range" n dq))
          ((< n (dq-lenf dq)) (list-ref (dq-f dq) n))
          (else (list-ref (dq-r dq) (- len n 1))))))

(: %ideque-take (ideque integer --> ideque))
(define (%ideque-take dq n)             ; n is within the range
  (let ((lenf (dq-lenf dq))
        (f    (dq-f dq)))
    (if (<= n lenf)
      (check n (take f n) 0 '())
      (let ((lenr. (- n lenf)))
        (check lenf f lenr. (take-right (dq-r dq) lenr.))))))

(: %ideque-drop (ideque integer --> ideque))
(define (%ideque-drop dq n)             ; n is within the range
  (let ((lenf (dq-lenf dq))
        (f    (dq-f dq))
        (lenr (dq-lenr dq))
        (r    (dq-r dq)))
    (if (<= n lenf)
      (check (- lenf n) (drop f n) lenr r)
      (let ((lenr. (- lenr (- n lenf))))
        (check 0 '() lenr. (take r lenr.))))))

(define (%check-length loc dq n)
  (unless (<= 0 n (ideque-length dq))
    (bounds-exception loc "length is out of range" n dq)))

;; API
(: ideque-take (ideque integer --> ideque))
(define (ideque-take dq n)
  (assert-type 'ideque-take (ideque? dq))
  (%check-length 'ideque-take dq n)
  (%ideque-take dq n))

;; API
(: ideque-take-right (ideque integer --> ideque))
(define (ideque-take-right dq n)
  (assert-type 'ideque-take-right (ideque? dq))
  (%check-length 'ideque-take-right dq n)
  (%ideque-drop dq (- (ideque-length dq) n)))

;; API
(: ideque-drop (ideque integer --> ideque))
(define (ideque-drop dq n)
  (assert-type 'ideque-drop (ideque? dq))
  (%check-length 'ideque-drop dq n)
  (%ideque-drop dq n))

;; API
(: ideque-drop-right (ideque integer --> ideque))
(define (ideque-drop-right dq n)
  (assert-type 'ideque-drop-right (ideque? dq))
  (%check-length 'ideque-drop-right dq n)
  (%ideque-take dq (- (ideque-length dq) n)))

;; API
;; FIXME: More efficient implementation.
(: ideque-split-at (ideque integer --> ideque ideque))
(define (ideque-split-at dq n)
  (assert-type 'ideque-split-at (ideque? dq))
  (%check-length 'ideque-split-at dq n)
  (values (%ideque-take dq n)
          (%ideque-drop dq n)))

;; API
(: ideque-length (ideque --> integer))
(define (ideque-length dq)
  (assert-type 'ideque-length (ideque? dq))
  (+ (dq-lenf dq) (dq-lenr dq)))

;; API
;; FIXME: Yikes.  More efficient implementation.
(: ideque-append (#!rest ideque --> ideque))
(define (ideque-append . dqs)
  (assert-type 'ideque-append (every ideque? dqs))
  ;; We could save some list copying by carefully split dqs into front and
  ;; rear groups and append separately, but for now we don't bother...
  (list->ideque (concatenate (map ideque->list dqs))))

;; API
(: ideque-count (procedure ideque --> integer))
(define (ideque-count pred dq)
  (assert-type 'ideque-count (ideque? dq))
  (+ (count pred (dq-f dq)) (count pred (dq-r dq))))

;; API
(: ideque-zip (ideque #!rest ideque --> ideque))
(define (ideque-zip dq . dqs)
  (assert-type 'ideque-zip (ideque? dq))
  (assert-type 'ideque-zip (every ideque? dqs))
  ;; An easy way.
  (let ((elts (apply zip (ideque->list dq) (map ideque->list dqs))))
    (check (length elts) elts 0 '())))

;; API
(: ideque-map (procedure ideque --> ideque))
(define (ideque-map proc dq)
  (assert-type 'ideque-map (procedure? proc))
  (assert-type 'ideque-map (ideque? dq))
  (%make-dq (dq-lenf dq) (map proc (dq-f dq))
            (dq-lenr dq) (map proc (dq-r dq))))

;; API
(: ideque-filter-map (procedure ideque --> ideque))
(define (ideque-filter-map proc dq)
  (assert-type 'ideque-filter-map (procedure? proc))
  (assert-type 'ideque-filter-map (ideque? dq))
  (let ((f (filter-map proc (dq-f dq)))
        (r (filter-map proc (dq-r dq))))
    (check (length f) f (length r) r)))

;; API
(: ideque-for-each (procedure ideque --> undefined))
(define (ideque-for-each proc dq)
  (assert-type 'ideque-for-each (procedure? proc))
  (assert-type 'ideque-for-each (ideque? dq))
  (for-each proc (dq-f dq))
  (for-each proc (reverse (dq-r dq))))

;; API
(: ideque-for-each-right (procedure ideque --> undefined))
(define (ideque-for-each-right proc dq)
  (assert-type 'ideque-for-each-right (procedure? proc))
  (assert-type 'ideque-for-each-right (ideque? dq))
  (for-each proc (dq-r dq))
  (for-each proc (reverse (dq-f dq))))

;; API
(: ideque-fold (procedure * ideque --> *))
(define (ideque-fold proc knil dq)
  (assert-type 'ideque-fold (procedure? proc))
  (assert-type 'ideque-fold (ideque? dq))
  (fold proc (fold proc knil (dq-f dq)) (reverse (dq-r dq))))

;; API
(: ideque-fold-right (procedure * ideque --> *))
(define (ideque-fold-right proc knil dq)
  (assert-type 'ideque-fold-right (procedure? proc))
  (assert-type 'ideque-fold-right (ideque? dq))
  (fold-right proc (fold-right proc knil (reverse (dq-r dq))) (dq-f dq)))

;; API
;; FIXME: Yikes.  More efficient implementation.
(: ideque-append-map (procedure ideque --> ideque))
(define (ideque-append-map proc dq)
  (assert-type 'ideque-append-map (procedure? proc))
  (assert-type 'ideque-append-map (ideque? dq))
  ;; can be cleverer, but for now...
  (list->ideque (append-map proc (ideque->list dq))))

(define (%ideque-filter-remove op pred dq)
  (let ((f (op pred (dq-f dq)))
        (r (op pred (dq-r dq))))
    (check (length f) f (length r) r)))

;; API
(: ideque-filter (procedure ideque --> ideque))
(define (ideque-filter pred dq)
  (assert-type 'ideque-filter (procedure? pred))
  (assert-type 'ideque-filter (ideque? dq))
  (%ideque-filter-remove filter pred dq))
(: ideque-remove (procedure ideque --> ideque))
(define (ideque-remove pred dq)
  (assert-type 'ideque-remove (procedure? pred))
  (assert-type 'ideque-remove (ideque? dq))
  (%ideque-filter-remove remove pred dq))

;; API
(: ideque-partition (procedure ideque --> ideque ideque))
(define (ideque-partition pred dq)
  (assert-type 'ideque-partition (procedure? pred))
  (assert-type 'ideque-partition (ideque? dq))
  (receive (f1 f2) (partition pred (dq-f dq))
    (receive (r1 r2) (partition pred (dq-r dq))
      (values (check (length f1) f1 (length r1) r1)
              (check (length f2) f2 (length r2) r2)))))

(define *not-found* (cons #f #f)) ; unique value

(: %search (procedure list list procedure --> *))
(define (%search pred seq1 seq2 failure)
  ;; We could write seek as CPS, but we employ *not-found* instead to avoid
  ;; closure allocation.
  (define (seek pred s)
    (cond ((null? s) *not-found*)
          ((pred (car s)) (car s))
          (else (seek pred (cdr s)))))
  (let ((r (seek pred seq1)))
    (if (not (eq? r *not-found*))
      r
      (let ((r (seek pred (reverse seq2))))
        (if (not (eq? r *not-found*))
          r
          (failure))))))

;; API
(: ideque-find (procedure ideque #!optional procedure --> *))
(define ideque-find
  (case-lambda
    ((pred dq) (ideque-find pred dq (lambda () #f)))
    ((pred dq failure)
     (assert-type 'ideque-find (procedure? pred))
     (assert-type 'ideque-find (ideque? dq))
     (assert-type 'ideque-find (procedure? failure))
     (%search pred (dq-f dq) (dq-r dq) failure))))

;; API
(: ideque-find-right (procedure ideque #!optional procedure --> *))
(define ideque-find-right
  (case-lambda
    ((pred dq) (ideque-find-right pred dq (lambda () #f)))
    ((pred dq failure)
     (assert-type 'ideque-find-right (procedure? pred))
     (assert-type 'ideque-find-right (ideque? dq))
     (assert-type 'ideque-find-right (procedure? failure))
     (%search pred (dq-r dq) (dq-f dq) failure))))

;; API
(: ideque-take-while (procedure ideque --> ideque))
(define (ideque-take-while pred dq)
  (assert-type 'ideque-take-while (procedure? pred))
  (assert-type 'ideque-take-while (ideque? dq))
  (receive (hd tl) (span pred (dq-f dq))
    (if (null? tl)
      (receive (hd. tl.) (span pred (reverse (dq-r dq)))
        (check (dq-lenf dq) (dq-f dq) (length hd.) (reverse hd.)))
      (check (length hd) hd 0 '()))))

;; API
;; FIXME: Yikes.  More efficient implementation.
(: ideque-take-while-right (procedure ideque --> ideque))
(define (ideque-take-while-right pred dq)
  (assert-type 'ideque-take-while-right (procedure? pred))
  (assert-type 'ideque-take-while-right (ideque? dq))
  (ideque-reverse (ideque-take-while pred (ideque-reverse dq))))

;; API
(: ideque-drop-while (procedure ideque --> ideque))
(define (ideque-drop-while pred dq)
  (assert-type 'ideque-drop-while (procedure? pred))
  (assert-type 'ideque-drop-while (ideque? dq))
  (receive (hd tl) (span pred (dq-f dq))
    (if (null? tl)
      (receive (hd. tl.) (span pred (reverse (dq-r dq)))
        (check (length tl.) tl. 0 '()))
      (check (length tl) tl (dq-lenr dq) (dq-r dq)))))

;; API
;; FIXME: Yikes.  More efficient implementation.
(: ideque-drop-while-right (procedure ideque --> ideque))
(define (ideque-drop-while-right pred dq)
  (assert-type 'ideque-drop-while-right (procedure? pred))
  (assert-type 'ideque-drop-while-right (ideque? dq))
  (ideque-reverse (ideque-drop-while pred (ideque-reverse dq))))

(: %idq-span-break (procedure procedure ideque --> ideque ideque))
(define (%idq-span-break op pred dq)
  (receive (head tail) (op pred (dq-f dq))
    (if (null? tail)
      (receive (head. tail.) (op pred (reverse (dq-r dq)))
        (values (check (length head) head (length head.) (reverse head.))
                (check (length tail.) tail. 0 '())))
      (values (check (length head) head 0 '())
              (check (length tail) tail (dq-lenr dq) (dq-r dq))))))

;; API
(: ideque-span (procedure ideque --> ideque ideque))
(define (ideque-span pred dq)
  (assert-type 'ideque-span (procedure? pred))
  (assert-type 'ideque-span (ideque? dq))
  (%idq-span-break span pred dq))
(: ideque-break (procedure ideque --> ideque ideque))
(define (ideque-break pred dq)
  (assert-type 'ideque-break (procedure? pred))
  (assert-type 'ideque-break (ideque? dq))
  (%idq-span-break break pred dq))

;; API
(: ideque-any (procedure ideque --> *))
(define (ideque-any pred dq)
  (assert-type 'ideque-any (procedure? pred))
  (assert-type 'ideque-any (ideque? dq))
  (if (null? (dq-r dq))
    (any pred (dq-f dq))
    (or (any pred (dq-f dq)) (any pred (reverse (dq-r dq))))))

;; API
(: ideque-every (procedure ideque --> *))
(define (ideque-every pred dq)
  (assert-type 'ideque-every (procedure? pred))
  (assert-type 'ideque-every (ideque? dq))
  (if (null? (dq-r dq))
    (every pred (dq-f dq))
    (and (every pred (dq-f dq)) (every pred (reverse (dq-r dq))))))

;; API
(: ideque->list (ideque --> list))
(define (ideque->list dq)
  (assert-type 'ideque->list (ideque? dq))
  (append (dq-f dq) (reverse (dq-r dq))))

;; API
(: list->ideque (list --> ideque))
(define (list->ideque lis)
  (assert-type 'list->ideque (or (pair? lis) (null? lis)))
  (check (length lis) lis 0 '()))

;; API
(: ideque->generator (ideque -> procedure))
(define (ideque->generator dq)
  (assert-type 'ideque->generator (ideque? dq))
  (lambda ()
    (if (ideque-empty? dq)
      #!eof
      (let ((v (ideque-front dq)))
        (set! dq (ideque-remove-front dq))
        v))))

;; API
(: generator->ideque (procedure -> ideque))
(define (generator->ideque gen)
  (assert-type 'generator->ideque (procedure? gen))
  (list->ideque (generator->list gen)))
