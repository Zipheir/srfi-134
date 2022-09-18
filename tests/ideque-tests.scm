;; SRFI 158 shim
(define (list->generator xs)
  (lambda ()
    (if (null? xs)
        #!eof
        (let ((x (car xs)))
          (set! xs (cdr xs))
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
    ((test-with-random-ideque var e0 e1 ...)
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
        (test (zip xs) (ideque->list (ideque-zip dq-x)))
        (test (zip xs ys) (ideque->list (ideque-zip dq-x dq-y)))
        (test (zip xs ys zs) (ideque->list (ideque-zip dq-x dq-y dq-z)))
      ))
    )
  )

(test-group "ideque/mapping"
  (test-group "ideque-map"
    (test-assert (ideque-empty? (ideque-map list (ideque))))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test-assert (ideque= eqv? dq (ideque-map values dq)))
        (test (make-list (ideque-length dq) 1)
              (ideque->list (ideque-map (constantly 1) dq)))
        (test (map - xs) (ideque->list (ideque-map - dq)))
        ))
    (test-assert (type-exception (ideque-map #t (ideque))))
    (test-assert (type-exception (ideque-map values 0)))
    )

  (test-group "ideque-filter-map"
    (test-assert (ideque-empty? (ideque-filter-map list (ideque))))
    (test-assert (ideque-empty? (ideque-filter-map values (ideque #f))))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test-assert (ideque= eqv? dq (ideque-filter-map values dq)))
        (test (make-list (ideque-length dq) 1)
              (ideque->list (ideque-filter-map (constantly 1) dq)))
        (test (map (lambda (x) (+ 1 x)) (filter odd? xs))
              (ideque->list
               (ideque-filter-map (lambda (x) (and (odd? x) (+ x 1)))
                                  dq)))
        ))
    (test-assert (type-exception (ideque-filter-map #t (ideque))))
    (test-assert (type-exception (ideque-filter-map values 0)))
    )

  (test-group "ideque-for-each"
    (test 0
          (let ((k 0))
            (ideque-for-each (lambda (_x) (set! k 1)) (ideque))
            k))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test (reverse xs)
              (let ((res '()))
                (ideque-for-each (lambda (x) (set! res (cons x res)))
                                 dq)
                res))
        (test (fold + 0 xs)
              (let ((sum 0))
                (ideque-for-each (lambda (x) (set! sum (+ sum x)))
                                 dq)
                sum))
        ))
    (test-assert (type-exception (ideque-for-each #t (ideque))))
    (test-assert (type-exception (ideque-for-each + 0)))
    )

  (test-group "ideque-for-each-right"
    (test 0
          (let ((k 0))
            (ideque-for-each-right (lambda (_x) (set! k 1)) (ideque))
            k))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test xs
              (let ((res '()))
                (ideque-for-each-right (lambda (x)
                                         (set! res (cons x res)))
                                       dq)
                res))
        (test (fold + 0 xs)
              (let ((sum 0))
                (ideque-for-each-right (lambda (x) (set! sum (+ sum x)))
                                       dq)
                sum))
        ))
    (test-assert (type-exception (ideque-for-each-right #t (ideque))))
    (test-assert (type-exception (ideque-for-each-right + 0)))
    )

  (test-group "ideque-fold"
    (test 'z (ideque-fold cons 'z (ideque)))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test (fold + 0 xs) (ideque-fold + 0 dq))
        (test (reverse xs) (ideque-fold cons '() dq))
        ))
    (test-assert (type-exception (ideque-fold #t 0 (ideque))))
    (test-assert (type-exception (ideque-fold cons '() #t)))
    )

  (test-group "ideque-fold-right"
    (test 'z (ideque-fold-right cons 'z (ideque)))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test (fold-right + 0 xs) (ideque-fold-right + 0 dq))
        (test xs (ideque-fold-right cons '() dq))
        ))
    (test-assert (type-exception (ideque-fold-right #t 0 (ideque))))
    (test-assert (type-exception (ideque-fold-right cons '() #t)))
    )

  (test-group "ideque-append-map"
    (test-assert (ideque-empty? (ideque-append-map values (ideque))))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test-assert (ideque= eqv? dq (ideque-append-map list dq)))
        (test (concatenate (zip xs xs))
              (ideque->list
               (ideque-append-map (lambda (x) (list x x)) dq)))
        ))
    (test-assert (type-exception (ideque-append-map #t (ideque))))
    (test-assert (type-exception (ideque-append-map values 0)))
    )
 )

(test-group "ideque/filtering"
  (test-group "ideque-filter"
    (test-assert (ideque-empty? (ideque-filter odd? (ideque))))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test-assert (ideque-empty?
                      (ideque-filter (constantly #f) dq)))
        (test-assert
         (ideque= eqv? dq (ideque-filter (constantly #t) dq)))
        (test-assert
         (ideque= eqv? dq (ideque-filter number?
                                         (ideque-add-front dq 'z))))
        (test (filter odd? xs) (ideque->list (ideque-filter odd? dq)))
        ))
    (test-assert (type-exception (ideque-filter 0 (ideque))))
    (test-assert (type-exception (ideque-filter odd? 0)))
    )

  (test-group "ideque-remove"
    (test-assert (ideque-empty? (ideque-remove odd? (ideque))))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test-assert (ideque-empty?
                      (ideque-remove (constantly #t) dq)))
        (test-assert
         (ideque= eqv? dq (ideque-remove (constantly #f) dq)))
        (test-assert
         (ideque= eqv? (ideque 'z)
                       (ideque-remove number?
                                      (ideque-add-front dq 'z))))
        (test (remove odd? xs) (ideque->list (ideque-remove odd? dq)))
        ))
    (test-assert (type-exception (ideque-remove 0 (ideque))))
    (test-assert (type-exception (ideque-remove odd? 0)))
    )

  (test-group "ideque-partition"
    (test-assert
     (let-values (((in out) (ideque-partition odd? (ideque))))
       (and (ideque-empty? in) (ideque-empty? out))))
    (test-with-random-lists (xs)
      (let ((dq (list->ideque xs)))
        (test-assert
         (let-values (((in out) (ideque-partition (constantly #t) dq)))
           (and (ideque= eqv? dq in) (ideque-empty? out))))
        (test-assert
         (let-values (((in out) (ideque-partition (constantly #f) dq)))
           (and (ideque-empty? in) (ideque= eqv? dq out))))
        (test-assert
         (let-values (((in out)
                       (ideque-partition number? (ideque-add-front dq 'z))))
           (and (ideque= eqv? dq in) (ideque= eqv? (ideque 'z) out))))
        (test-assert
         (let-values ((dqs (ideque-partition odd? dq))
                      (parts (partition odd? xs)))
           (equal? parts (map ideque->list dqs))))
        ))
    (test-assert (type-exception (ideque-partition 0 (ideque))))
    (test-assert (type-exception (ideque-partition odd? 0)))
    )
  )

(test-group "ideque/searching"
  (test-group "ideque-find"
    (test #f (ideque-find (constantly #t) (ideque)))
    (test-with-random-ideque dq
      (test 'z (ideque-find (constantly #f) dq (lambda () 'z)))
      (test-assert
       (or (ideque-empty? dq)
           (eqv? (ideque-front dq) (ideque-find (constantly #t) dq))))
      (test 'z (ideque-find string? dq (lambda () 'z)))
      (test 'shoop (ideque-find symbol?
                                (ideque-add-front
                                 (ideque-add-back dq 'woop)
                                 'shoop)))
      )
    (test-assert (type-exception (ideque-find 0 (ideque))))
    (test-assert (type-exception (ideque-find odd? 0)))
    )

  (test-group "ideque-find-right"
    (test #f (ideque-find-right (constantly #t) (ideque)))
    (test-with-random-ideque dq
      (test 'z (ideque-find-right (constantly #f) dq (lambda () 'z)))
      (test-assert
       (or (ideque-empty? dq)
           (eqv? (ideque-back dq) (ideque-find-right (constantly #t) dq))))
      (test 'z (ideque-find-right string? dq (lambda () 'z)))
      (test 'woop (ideque-find-right symbol?
                                     (ideque-add-front
                                      (ideque-add-back dq 'woop)
                                     'shoop)))
      )
    (test-assert (type-exception (ideque-find-right 0 (ideque))))
    (test-assert (type-exception (ideque-find-right odd? 0)))
    )

  (test-group "ideque-take-while"
    (test-assert (ideque-empty? (ideque-take-while (constantly #t)
                                                   (ideque))))
    (test-with-random-ideque dq
      (test-assert (ideque-empty? (ideque-take-while (constantly #f) dq)))
      (test-assert (ideque= eqv? dq (ideque-take-while (constantly #t) dq)))
      (test-assert
       (ideque= eqv?
                (ideque 'z)
                (ideque-take-while symbol? (ideque-add-front dq 'z))))
      (test-assert
       (ideque= eqv?
                dq
                (ideque-take-while number? (ideque-add-back dq 'z))))
      )
    (test-assert (type-exception (ideque-take-while 0 (ideque))))
    (test-assert (type-exception (ideque-take-while odd? 0)))
    )

  (test-group "ideque-drop-while"
    (test-assert (ideque-empty? (ideque-drop-while (constantly #t)
                                                   (ideque))))
    (test-with-random-ideque dq
      (test-assert (ideque-empty? (ideque-drop-while (constantly #t) dq)))
      (test-assert (ideque= eqv? dq (ideque-drop-while (constantly #f) dq)))
      (test-assert
       (ideque= eqv?
                dq
                (ideque-drop-while symbol? (ideque-add-front dq 'z))))
      (test-assert
       (ideque= eqv?
                (ideque 'z)
                (ideque-drop-while number? (ideque-add-back dq 'z))))
      )
    (test-assert (type-exception (ideque-drop-while 0 (ideque))))
    (test-assert (type-exception (ideque-drop-while odd? 0)))
    )

  (test-group "ideque-take-while-right"
    (test-assert
     (ideque-empty? (ideque-take-while-right (constantly #t) (ideque))))
    (test-with-random-ideque dq
      (test-assert
       (ideque-empty? (ideque-take-while-right (constantly #f) dq)))
      (test-assert
       (ideque= eqv? dq (ideque-take-while-right (constantly #t) dq)))
      (test-assert
       (ideque= eqv?
                (ideque 'z)
                (ideque-take-while-right symbol? (ideque-add-back dq 'z))))
      (test-assert
       (ideque= eqv?
                dq
                (ideque-take-while-right number? (ideque-add-front dq 'z))))
      )
    (test-assert (type-exception (ideque-take-while-right 0 (ideque))))
    (test-assert (type-exception (ideque-take-while-right odd? 0)))
    )

  (test-group "ideque-drop-while-right"
    (test-assert
     (ideque-empty? (ideque-drop-while-right (constantly #t) (ideque))))
    (test-with-random-ideque dq
      (test-assert
       (ideque-empty? (ideque-drop-while-right (constantly #t) dq)))
      (test-assert
       (ideque= eqv? dq (ideque-drop-while-right (constantly #f) dq)))
      (test-assert
       (ideque= eqv?
                dq
                (ideque-drop-while-right symbol? (ideque-add-back dq 'z))))
      (test-assert
       (ideque= eqv?
                (ideque 'z)
                (ideque-drop-while-right number? (ideque-add-front dq 'z))))
      )
    (test-assert (type-exception (ideque-drop-while-right 0 (ideque))))
    (test-assert (type-exception (ideque-drop-while-right odd? 0)))
    )

  (test-group "ideque-span"
    (test-assert
     (let-values (((dq1 dq2) (ideque-span (constantly #t) (ideque))))
       (and (ideque-empty? dq1) (ideque-empty? dq2))))
    (test-with-random-ideque dq
      (test-assert
       (let-values (((dq1 dq2) (ideque-span (constantly #f) dq)))
         (and (ideque-empty? dq1) (ideque= eqv? dq dq2))))
      (test-assert
       (let-values (((dq1 dq2) (ideque-span (constantly #t) dq)))
         (and (ideque= eqv? dq dq1) (ideque-empty? dq2))))
      (test-assert
       (let-values (((dq1 dq2)
                     (ideque-span symbol? (ideque-add-front dq 'z))))
         (and (ideque= eqv? (ideque 'z) dq1) (ideque= eqv? dq dq2))))
      (test-assert
       (let-values (((dq1 dq2)
                     (ideque-span number? (ideque-add-back dq 'z))))
         (and (ideque= eqv? dq dq1) (ideque= eqv? (ideque 'z) dq2))))
      )
    (test-assert (type-exception (ideque-span 0 (ideque))))
    (test-assert (type-exception (ideque-span odd? 0.2)))
    )

  (test-group "ideque-break"
    (test-assert
     (let-values (((dq1 dq2) (ideque-break (constantly #t) (ideque))))
       (and (ideque-empty? dq1) (ideque-empty? dq2))))
    (test-with-random-ideque dq
      (test-assert
       (let-values (((dq1 dq2) (ideque-break (constantly #f) dq)))
         (and (ideque= eqv? dq dq1) (ideque-empty? dq2))))
      (test-assert
       (let-values (((dq1 dq2) (ideque-break (constantly #t) dq)))
         (and (ideque-empty? dq1) (ideque= eqv? dq dq2))))
      (test-assert
       (let-values (((dq1 dq2)
                     (ideque-break symbol? (ideque-add-back dq 'z))))
         (and (ideque= eqv? dq dq1) (ideque= eqv? (ideque 'z) dq2))))
      (test-assert
       (let-values (((dq1 dq2)
                     (ideque-break number? (ideque-add-front dq 'z))))
         (and (ideque= eqv? (ideque 'z) dq1) (ideque= eqv? dq dq2))))
      )
    (test-assert (type-exception (ideque-break 0 (ideque))))
    (test-assert (type-exception (ideque-break odd? 0.2)))
    )
  )

(test-group "ideque/conversions"
  ;; 90% of the above tests lean on the list->ideque conversion, so
  ;; these are pretty limited.
  (test-group "list->ideque"
    (test-assert (ideque-empty? (list->ideque '())))
    (let ((dq (list->ideque '(1 2 3))))
      (test 3 (ideque-length dq))
      (test 1 (ideque-front dq))
      (test 2 (ideque-front (ideque-remove-front dq)))
      (test 2 (ideque-back (ideque-remove-back dq)))
      (test 3 (ideque-back dq)))
    (test-assert (type-exception (list->ideque '(0 . 0))))
    )

  (test-group "ideque->generator"
    (test-assert (null? (generator->list (ideque->generator (ideque)))))
    (test-with-random-lists (xs)
      (test xs (generator->list (ideque->generator (list->ideque xs))))
      )
    (test-assert (type-exception (ideque->generator 0.2)))
    )

  (test-group "generator->ideque"
    (test-assert
     (ideque-empty? (generator->ideque (list->generator '()))))
    (test-with-random-lists (xs)
      (test xs (ideque->list (generator->ideque (list->generator xs))))
      )
    (test-assert (type-exception (generator->ideque 0.2)))
    )
  )
