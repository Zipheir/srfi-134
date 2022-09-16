;; SRFI 158 shim
(define (generator . args)
  (lambda ()
    (if (null? args)
        #!eof
        (let ((x (car args)))
          (set! args (cdr args))
          x))))

(define (generator->list gen)
  (letrec
   ((build
     (lambda (xs)
       (let ((x (gen)))
         (if (eof-object? x)
             (reverse xs)
             (build (cons x xs)))))))
    (build '())))

(define length-bound 64)

(define (random-fixnum-list)
  (list-tabulate (pseudo-random-integer length-bound)
                 (lambda (junk)
                   (pseudo-random-integer 32768))))

(define (random-fixnum-ideque)
  (list->ideque (random-fixnum-list)))

(define-syntax test-with-random-lists
  (ir-macro-transformer
   (lambda (exp _inject _compare)
     `(test-generative ,(map (lambda (v)
                               `(,v (lambda () (random-fixnum-list))))
                             (cadr exp))
        ,@(cddr exp)))))

(define-syntax test-with-random-ideque
  (syntax-rules ()
    ((test-with-random-list var e0 e1 ...)
     (test-generative ((var random-fixnum-ideque))
       e0 e1 ...))))

;; Evaluates to true if the expression raises a type condition.
(define-syntax type-exception
  (syntax-rules ()
    ((type-exception e)
     (handle-exceptions con
                        ((condition-predicate 'type) con)
       e))))

;; Evaluates to true if the expression raises a bounds condition.
(define-syntax bounds-exception
  (syntax-rules ()
    ((type-exception e)
     (handle-exceptions con
                        ((condition-predicate 'bounds) con)
       e))))

;;;; Tests

(test-group "ideque/constructors"
  (test-group "ideque"
    (test '() (ideque->list (ideque)))
    (test-with-random-lists (xs)
      (test xs (ideque->list (apply ideque xs)))
      )
    )

  (test-group "list->ideque"
    (test '() (ideque->list (list->ideque '())))
    (test-with-random-lists (xs)
      (test xs (ideque->list (list->ideque xs)))
      )
    (test-assert (type-exception (list->ideque #t)))
    )

  (test-group "ideque-unfold"
    (test '() (ideque->list
               (ideque-unfold (lambda (n) #t)
                              values
                              (lambda (n) (+ n 1))
                              0)))
    (test '(10 9 8 7 6 5 4 3 2 1)
          (ideque->list (ideque-unfold zero?
                                       values
                                       (lambda (n) (- n 1)) 10)))
    (test-with-random-lists (xs)
      (test xs (ideque->list (ideque-unfold null? car cdr xs)))
      )
    (test-assert (type-exception (ideque-unfold #t car cdr #f)))
    (test-assert (type-exception (ideque-unfold null? #t cdr #f)))
    (test-assert (type-exception (ideque-unfold null? car #t #f)))
    )

  (test-group "ideque-unfold-right"
    (test '() (ideque->list
               (ideque-unfold-right (lambda (n) #t)
                                    values
                                    (lambda (n) (+ n 1))
                                    0)))
    (test '(1 2 3 4 5 6 7 8 9 10)
          (ideque->list
           (ideque-unfold-right zero? values (lambda (n) (- n 1)) 10)))
    (test-with-random-lists (xs)
      (test (reverse xs)
            (ideque->list (ideque-unfold-right null? car cdr xs)))
      )
    (test-assert (type-exception (ideque-unfold-right #t car cdr #f)))
    (test-assert (type-exception (ideque-unfold-right null? #t cdr #f)))
    (test-assert (type-exception (ideque-unfold-right null? car #t #f)))
    )

  (test-group "ideque-tabulate"
    (test '() (ideque->list (ideque-tabulate 0 values)))
    (test '(0 2 4 6 8 10)
          (ideque->list (ideque-tabulate 6 (lambda (n) (* n 2)))))
    (test-with-random-lists (xs)
      (test xs
            (ideque->list
             (ideque-tabulate (length xs) (cut list-ref xs <>))))
      )
    (test-assert (type-exception (ideque-tabulate #t values)))
    (test-assert (type-exception (ideque-tabulate 10 #t)))
    ) 
  )

(test-group "ideque/predicates"
  (test-group "ideque?"
    (test-assert (not (ideque? 1)))
    (test-with-random-ideque dq
      (test-assert (ideque? dq)))
    )

  (test-group "ideque-empty?"
    (test-assert (ideque-empty? (ideque)))
    (test-with-random-lists (xs)
      (test-assert (if (null? xs)
                       (ideque-empty? (list->ideque xs))
                       (not (ideque-empty? (list->ideque xs))))))
    (test-assert (type-exception (ideque-empty? 0)))
    )

  (test-group "ideque="
    (test-assert (ideque= eq?))
    (test-assert (ideque= eq? (ideque 1)))
    (test-assert (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B)))
    (test-assert (ideque= char-ci=? (ideque) (ideque)))
    (test-assert (not (ideque= char-ci=?
                               (ideque #\a #\b)
                               (ideque #\A #\B #\c))))
    (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A))))
    (test-assert (ideque= char-ci=? (ideque) (ideque) (ideque)))
    (test-assert (ideque= char-ci=? (ideque #\a #\b)
                                    (ideque #\A #\B)
                                    (ideque #\a #\B)))
    (test-assert (not (ideque= char-ci=? (ideque #\a #\b)
                                         (ideque #\A)
                                         (ideque #\a #\B))))
    (test-assert (not (ideque= char-ci=? (ideque #\a #\b)
                                         (ideque #\A #\B)
                                         (ideque #\A #\B #\c))))

    (test-with-random-ideque dq
      (let ((dq* (ideque-add-front dq 1)))
        (test-assert (ideque= eqv? dq dq))
        (test-assert (ideque= eqv? (ideque-add-front dq 3)  ; not eq?
                                   (ideque-add-front dq 3)))
        (test #f (ideque= eqv? dq dq*))
        (test #f (ideque= eqv? dq* dq))
        (test-assert (ideque= eqv? dq dq dq))
        (test #f (ideque= eqv? dq* dq dq))
        (test #f (ideque= eqv? dq dq* dq))
        (test #f (ideque= eqv? dq dq dq*))
        ))
    (test-assert (type-exception (ideque= 0)))
    (test-assert (type-exception (ideque= eqv? 0)))
    (test-assert (type-exception (ideque= eqv? (ideque) 0)))
    (test-assert (type-exception (ideque= eqv? (ideque) (ideque) 0)))
    )

  (test-group "ideque-any"
    (test 3 (ideque-any (lambda (x) (and (number? x) x))
                        (ideque 'a 3 'b 'c 4 'd 'e)))
    (test 5 (ideque-any (lambda (x) (and (number? x) x))
                        (ideque 'a 'b 'c 'd 'e 5)))
    (test #f (ideque-any (lambda (x) (and (number? x) x))
                         (ideque 'a 'b 'c 'd 'e)))
    ;; check if we won't see further once we found the result
    (test 1 (ideque-any (lambda (x) (and (odd? x) x))
                        (ideque 2 1 'a 'b 'c 'd)))

    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test (pair? xs) (ideque-any (constantly #t) dq))
        (test #f (ideque-any (constantly #f) dq))

        (test (any odd? xs) (ideque-any odd? dq))
        (test (any (lambda (x) (and (even? x) x)) xs)
              (ideque-any (lambda (x) (and (even? x) x)) dq))
        ))
    (test-assert (type-exception (ideque-any #t (ideque))))
    (test-assert (type-exception (ideque-any odd? #t)))
    )

  (test-group "ideque-every"
    (test 9 (ideque-every (lambda (x) (and (number? x) x))
                          (ideque 1 5 3 2 9)))
    (test #f (ideque-every (lambda (x) (and (number? x) x))
                           (ideque 1 5 'a 2 9)))
    ;; check if we won't see further once we found the result
    (test #f (ideque-every (lambda (x) (and (odd? x) x))
                           (ideque 1 2 'a 'b 'c 'd)))
    )

    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test (null? xs) (ideque-every (constantly #f) dq))
        (test #t (ideque-every (constantly #t) dq))

        (test (every odd? xs) (ideque-every odd? dq))
        (test (every (lambda (x) (and (even? x) x)) xs)
              (ideque-every (lambda (x) (and (even? x) x)) dq))
      ))
    (test-assert (type-exception (ideque-every #t (ideque))))
    (test-assert (type-exception (ideque-every odd? #t)))
  )

(test-group "ideque/queue-operations"
  (test-error (ideque-front (ideque)))
  (test-error (ideque-back (ideque)))
  (test 1 (ideque-front (ideque 1 2 3)))
  (test 3 (ideque-back (ideque 1 2 3)))
  (test 2 (ideque-front (ideque-remove-front (ideque 1 2 3))))
  (test 2 (ideque-back (ideque-remove-back (ideque 1 2 3))))
  (test 1 (ideque-front (ideque-remove-back (ideque 1 2 3))))
  (test 3 (ideque-back (ideque-remove-front (ideque 1 2 3))))
  (test-assert (ideque-empty? (ideque-remove-front (ideque 1))))
  (test-assert (ideque-empty? (ideque-remove-back (ideque 1))))
  (test 0 (ideque-front (ideque-add-front (ideque 1 2 3) 0)))
  (test 0 (ideque-back (ideque-add-back (ideque 1 2 3) 0)))
  (test 0 (ideque-front (ideque-add-back (ideque) 0)))
  (test 0 (ideque-back (ideque-add-front (ideque) 0)))
  ;; loss of front ideque
  (let ((id (list->ideque (make-list 18 #f))))
    (set! id (ideque-remove-front (ideque-add-back id 1)))
    (set! id (ideque-remove-front (ideque-add-back id 1)))
    (set! id (ideque-remove-front (ideque-add-back id 1)))
    (test #f (ideque-front (ideque-take-right id 12))))
  )

(test-group "ideque/other-accessors"
  (test-group "ideque-take"
    (test-assert (ideque-empty? (ideque-take (ideque) 0)))
    (test-with-random-lists (xs)
      (let* ((dq (list->ideque xs))
             (len (ideque-length dq))
             (k (quotient len 2)))
        (test-assert (ideque-empty? (ideque-take dq 0)))
        (test xs (ideque->list (ideque-take dq len)))
        (test (take xs k) (ideque->list (ideque-take dq k)))
        (test-assert (bounds-exception (ideque-take dq (+ len 1))))
        ))
    (test-assert (type-exception (ideque-take #t 0)))
    (test-assert (type-exception (ideque-take (ideque) 0.3)))
    )

  (test-group "ideque-take-right"
    (test-assert (ideque-empty? (ideque-take-right (ideque) 0)))
    (test-with-random-lists (xs)
      (let* ((dq (list->ideque xs))
             (len (ideque-length dq))
             (k (quotient len 2)))
        (test-assert (ideque-empty? (ideque-take-right dq 0)))
        (test xs (ideque->list (ideque-take-right dq len)))
        (test (take-right xs k) (ideque->list (ideque-take-right dq k)))
        (test-assert (bounds-exception (ideque-take-right dq (+ len 1))))
        ))
    (test-assert (type-exception (ideque-take-right #t 0)))
    (test-assert (type-exception (ideque-take-right (ideque) 0.3)))
    )

  (test-group "ideque-drop"
    (test-assert (ideque-empty? (ideque-drop (ideque) 0)))
    (test-with-random-lists (xs)
      (let* ((dq (list->ideque xs))
             (len (ideque-length dq))
             (k (quotient len 2)))
        (test-assert (ideque-empty? (ideque-drop dq len)))
        (test xs (ideque->list (ideque-drop dq 0)))
        (test (drop xs k) (ideque->list (ideque-drop dq k)))
        (test-assert (bounds-exception (ideque-drop dq (+ len 1))))
        ))
    (test-assert (type-exception (ideque-drop #t 0)))
    (test-assert (type-exception (ideque-drop (ideque) 0.3)))
    )

  (test-group "ideque-drop-right"
    (test-assert (ideque-empty? (ideque-drop-right (ideque) 0)))
    (test-with-random-lists (xs)
      (let* ((dq (list->ideque xs))
             (len (ideque-length dq))
             (k (quotient len 2)))
        (test-assert (ideque-empty? (ideque-drop-right dq len)))
        (test xs (ideque->list (ideque-drop-right dq 0)))
        (test (drop-right xs k) (ideque->list (ideque-drop-right dq k)))
        (test-assert (bounds-exception (ideque-drop-right dq (+ len 1))))
        ))
    (test-assert (type-exception (ideque-drop-right #t 0)))
    (test-assert (type-exception (ideque-drop-right (ideque) 0.3)))
    )

  (test-group "ideque-split-at"
    (test-assert
     (let-values (((dq1 dq2) (ideque-split-at (ideque) 0)))
       (and (ideque-empty? dq1) (ideque-empty? dq2))))
    (test-with-random-lists (xs)
      (let* ((dq (list->ideque xs))
             (len (ideque-length dq))
             (k (quotient len 2)))
        (test-assert
         (let-values (((head tail) (ideque-split-at dq 0)))
           (and (ideque-empty? head)
                (equal? xs (ideque->list tail)))))
        (test-assert
         (let-values (((head tail) (ideque-split-at dq len)))
           (and (equal? xs (ideque->list head))
                (ideque-empty? tail))))
        (test-assert
         (let-values (((h-lis t-lis) (split-at xs k))
                      ((h-dq t-dq) (ideque-split-at dq k)))
           (and (equal? h-lis (ideque->list h-dq))
                (equal? t-lis (ideque->list t-dq)))))
        (test-assert (bounds-exception (ideque-split-at dq (+ len 1))))
        ))
    (test-assert (type-exception (ideque-split-at #t 0)))
    (test-assert (type-exception (ideque-split-at (ideque) 0.3)))
    )

  (test-group "ideque-ref"
    (test-with-random-ideque dq
      (let ((len (ideque-length dq)))
        (test (ideque->list dq)
              (list-tabulate len (lambda (i) (ideque-ref dq i))))
        (test 1 (ideque-ref (ideque-add-front dq 1) 0))
        (test 99 (ideque-ref (ideque-add-back dq 99) len))
        (test-assert (bounds-exception (ideque-ref dq (+ len 1))))
      ))
    (test-assert (type-exception (ideque-ref 0 0)))
    (test-assert (type-exception (ideque-ref (ideque) 0.2)))
    )
  )

(test-group "ideque/whole-ideque"
  (test-group "ideque-length"
    (test 7 (ideque-length (ideque 1 2 3 4 5 6 7)))
    (test 0 (ideque-length (ideque)))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs))
            (len (length xs)))
        (test len (ideque-length dq))
        (test (+ len 1) (ideque-length (ideque-add-front dq 1)))
        (test (+ len 1) (ideque-length (ideque-add-back dq 1)))
        (test-assert
         (or (zero? len)
             (= (- len 1) (ideque-length (ideque-remove-front dq)))))
        (test-assert
         (or (zero? len)
             (= (- len 1) (ideque-length (ideque-remove-back dq)))))
        ))
    (test-assert (type-exception (ideque-length '())))
    )

  (test-group "ideque-append"
    (test-assert (ideque-empty? (ideque-append)))
    (test-assert (ideque-empty? (ideque-append (ideque))))
    (test-assert (ideque-empty? (ideque-append (ideque) (ideque))))

    (test-with-random-lists (xs ys zs)
      (let ((dq-x (list->ideque xs))
            (dq-y (list->ideque ys))
            (dq-z (list->ideque zs)))
        (test xs (ideque->list (ideque-append dq-x)))
        (test xs (ideque->list (ideque-append (ideque) dq-x)))
        (test xs (ideque->list (ideque-append (ideque) dq-x (ideque))))
        (test (append xs ys) (ideque->list (ideque-append dq-x dq-y)))
        (test (append xs ys)
              (ideque->list (ideque-append dq-x (ideque) dq-y)))
        (test (append xs ys zs)
              (ideque->list (ideque-append dq-x dq-y dq-z)))
        (test (append xs ys zs)
              (ideque->list (ideque-append (ideque)
                                           dq-x
                                           dq-y
                                           (ideque)
                                           dq-z
                                           (ideque))))
        ))
    (test-assert (type-exception (ideque-append '(1))))
    (test-assert (type-exception (ideque-append (ideque) #t)))
    (test-assert (type-exception (ideque-append (ideque) (ideque) 0)))
    )

  (test-group "ideque-reverse"
    (test-assert (ideque-empty? (ideque-reverse (ideque))))
    (test-with-random-lists (xs)
      (test (reverse xs)
            (ideque->list (ideque-reverse (list->ideque xs))))
      )
    (test-assert (type-exception (ideque-reverse '())))
    )

  (test-group "ideque-count"
    (test 0 (ideque-count (constantly #t) (ideque)))
    (test-with-random-ideque dq
      (test 0 (ideque-count (constantly #f) dq))
      (test (ideque-length dq) (ideque-count (constantly #t) dq))
      (test (count odd? (ideque->list dq)) (ideque-count odd? dq))
      )
    (test-assert (type-exception (ideque-count #t (ideque))))
    (test-assert (type-exception (ideque-count odd? 0)))
    )

  (test-group "ideque-zip"
    (test-assert (ideque-empty? (ideque-zip (ideque) (ideque))))
    (test-assert (ideque-empty? (ideque-zip (ideque 1) (ideque))))
    (test-assert (ideque-empty? (ideque-zip (ideque) (ideque 1))))
    (test-with-random-lists (xs ys zs)
      (let ((dq-x (list->ideque xs))
            (dq-y (list->ideque ys))
            (dq-z (list->ideque zs)))
        (test (zip xs ys) (ideque->list (ideque-zip dq-x dq-y)))
        (test (zip xs ys zs) (ideque->list (ideque-zip dq-x dq-y dq-z)))
      ))
    )
  )

(test-group "ideque/mapping"
 (test-assert (ideque-empty? (ideque-map list (ideque))))
 (test '(-1 -2 -3 -4 -5) (ideque->list (ideque-map - (ideque 1 2 3 4 5))))
 (test '(-1 -3 5 -8)
       (ideque->list (ideque-filter-map (lambda (x) (and (number? x) (- x)))
                                        (ideque 1 3 'a -5 8))))
 (test '(5 4 3 2 1)
       (let ((r '()))
         (ideque-for-each (lambda (n) (set! r (cons n r)))
                          (ideque 1 2 3 4 5))
         r))
 (test '(1 2 3 4 5)
       (let ((r '()))
         (ideque-for-each-right (lambda (n) (set! r (cons n r)))
                                (ideque 1 2 3 4 5))
         r))
 (test '(5 4 3 2 1 . z)
       (ideque-fold cons 'z (ideque 1 2 3 4 5)))
 (test '(1 2 3 4 5 . z)
       (ideque-fold-right cons 'z (ideque 1 2 3 4 5)))
 (test '(a a b b c c)
       (ideque->list (ideque-append-map (lambda (x) (list x x))
                                        (ideque 'a 'b 'c))))
 )

(test-group "ideque/filtering"
 (test '(1 3 5)
       (ideque->list (ideque-filter odd? (ideque 1 2 3 4 5))))
 (test '(2 4)
       (ideque->list (ideque-remove odd? (ideque 1 2 3 4 5))))
 (test '((1 3 5) (2 4))
       (receive xs (ideque-partition odd? (ideque 1 2 3 4 5))
                (map ideque->list xs)))
 )

(test-group "ideque/searching"
 (test 3 (ideque-find number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo)))
 (test 'boo (ideque-find number? (ideque 'a 'b 'c 'd) (lambda () 'boo)))
 (test #f (ideque-find number? (ideque 'a 'b 'c 'd)))
 (test 4 (ideque-find-right number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo)))
 (test 'boo (ideque-find-right number? (ideque 'a 'b 'c 'd) (lambda () 'boo)))
 (test #f (ideque-find-right number? (ideque 'a 'b 'c 'd)))
 (test '(1 3 2)
       (ideque->list (ideque-take-while (lambda (n) (< n 5))
                                        (ideque 1 3 2 5 8 4 6 3 4 2))))
 (test '(5 8 4 6 3 4 2)
       (ideque->list (ideque-drop-while (lambda (n) (< n 5))
                                        (ideque 1 3 2 5 8 4 6 3 4 2))))
 (test '(3 4 2)
       (ideque->list (ideque-take-while-right (lambda (n) (< n 5))
                                              (ideque 1 3 2 5 8 4 6 3 4 2))))
 (test '(1 3 2 5 8 4 6)
       (ideque->list (ideque-drop-while-right (lambda (n) (< n 5))
                                              (ideque 1 3 2 5 8 4 6 3 4 2))))
 (test '()
       (ideque->list (ideque-take-while (lambda (n) (< n 5))
                                        (ideque 5 8 4 6 3 4 2 9))))
 (test '()
       (ideque->list (ideque-drop-while (lambda (n) (< n 5))
                                        (ideque 1 4 3 2 3 4 2 1))))
 (test '()
       (ideque->list (ideque-take-while-right (lambda (n) (< n 5))
                                              (ideque 5 8 4 6 3 4 2 9))))
 (test '()
       (ideque->list (ideque-drop-while-right (lambda (n) (< n 5))
                                              (ideque 1 3 2 4 3 2 3 2))))
 (test '((1 3 2) (5 8 4 6 3 4 2))
       (receive xs (ideque-span (lambda (n) (< n 5))
                                (ideque 1 3 2 5 8 4 6 3 4 2))
                (map ideque->list xs)))
 (test '((5 8) (4 6 3 4 2 9))
       (receive xs (ideque-break (lambda (n) (< n 5))
                                 (ideque 5 8 4 6 3 4 2 9))
                (map ideque->list xs)))
 )

(test-group "ideque/conversions"
 (test '(1 2 3) (generator->list (ideque->generator (ideque 1 2 3))))
 (test '() (generator->list (ideque->generator (ideque))))
 (test '(1 2 3) (ideque->list (generator->ideque (generator 1 2 3))))
 (test '() (ideque->list (generator->ideque (generator))))
 (test #t (stream-null? (ideque->stream (ideque))))
 (test '(1 2 3) (stream->list (ideque->stream (ideque 1 2 3))))
 (test #t (ideque-empty? (stream->ideque stream-null)))
 (test '(1 2 3) (ideque->list (stream->ideque (list->stream '(1 2 3)))))
 )
