;; Requires srfi-1 and srfi-121 (generators)

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

(define-syntax test-with-random-list
  (syntax-rules ()
    ((test-with-random-list var e0 e1 ...)
     (test-generative ((var random-fixnum-list))
       e0 e1 ...))))

;; Evaluates to true if the expression raises a type condition.
(define-syntax type-exception
  (syntax-rules ()
    ((type-exception e)
     (handle-exceptions con
                        ((condition-predicate 'type) con)
       e))))

(test-group "ideque/constructors"
  (test-group "ideque"
    (test '() (ideque->list (ideque)))
    (test-with-random-list xs
      (test xs (ideque->list (apply ideque xs)))
      )
    )

  (test-group "list->ideque"
    (test '() (ideque->list (list->ideque '())))
    (test-with-random-list xs
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
    (test-with-random-list xs
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
    (test-with-random-list xs
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
    (test-with-random-list xs
      (test xs
            (ideque->list
             (ideque-tabulate (length xs) (cut list-ref xs <>))))
      )
    (test-assert (type-exception (ideque-tabulate #t values)))
    (test-assert (type-exception (ideque-tabulate 10 #t)))
    ) 
  )

(test-group "ideque/predicates"
 (test-assert (ideque? (ideque)))
 (test-assert (not (ideque? 1)))
 (test-assert (ideque-empty? (ideque)))
 (test-assert (not (ideque-empty? (ideque 1))))
 (test-assert (ideque= eq?))
 (test-assert (ideque= eq? (ideque 1)))
 (test-assert (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B)))
 (test-assert (ideque= char-ci=? (ideque) (ideque)))
 (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B #\c))))
 (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A))))
 (test-assert (ideque= char-ci=? (ideque) (ideque) (ideque)))
 (test-assert (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B) (ideque #\a #\B)))
 (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A) (ideque #\a #\B))))
 (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B) (ideque #\A #\B #\c))))
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
 ;; loss of front ideque
 (let ((id (ideque #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
  (set! id (ideque-remove-front (ideque-add-back id 1)))
  (set! id (ideque-remove-front (ideque-add-back id 1)))
  (set! id (ideque-remove-front (ideque-add-back id 1)))
  (test #f (ideque-front (ideque-take-right id 12))))
)

(test-group "ideque/other-accessors"
 (test '(1 2 3 4) (ideque->list (ideque-take (ideque 1 2 3 4) 4)))
 (test '(1 2 3 4) (ideque->list (ideque-take-right (ideque 1 2 3 4) 4)))
 (test '() (ideque->list (ideque-drop (ideque 1 2 3 4) 4)))
 (test '() (ideque->list (ideque-drop-right (ideque 1 2 3 4) 4)))
 ;; out-of-range conditions
 (test-error (ideque->list (ideque-take (ideque 1 2 3 4 5 6 7) 10)))
 (test-error (ideque->list (ideque-take-right (ideque 1 2 3 4 5 6 7) 10)))
 (test-error (ideque-split-at (ideque 1 2 3 4 5 6 7) 10))

 (test '(3 2 1) (map (lambda (n) (ideque-ref (ideque 3 2 1) n)) '(0 1 2)))
 (test-error (ideque-ref (ideque 3 2 1) -1))
 (test-error (ideque-ref (ideque 3 2 1) 3))
 )

(test-group "ideque/whole-ideque"
 (test 7 (ideque-length (ideque 1 2 3 4 5 6 7)))
 (test 0 (ideque-length (ideque)))
 (test '() (ideque->list (ideque-append)))
 (test '() (ideque->list (ideque-append (ideque) (ideque))))
 (test '(1 2 3 a b c d 5 6 7 8 9)
       (ideque->list (ideque-append (ideque 1 2 3)
                                    (ideque 'a 'b 'c 'd)
                                    (ideque)
                                    (ideque 5 6 7 8 9))))
 (test '() (ideque->list (ideque-reverse (ideque))))
 (test '(5 4 3 2 1) (ideque->list (ideque-reverse (ideque 1 2 3 4 5))))
 (test 0 (ideque-count odd? (ideque)))
 (test 3 (ideque-count odd? (ideque 1 2 3 4 5)))
 (test '((1 a) (2 b) (3 c))
       (ideque->list (ideque-zip (ideque 1 2 3) (ideque 'a 'b 'c 'd 'e))))
 (test '((1 a x) (2 b y) (3 c z))
       (ideque->list (ideque-zip (ideque 1 2 3 4 5)
                                 (ideque 'a 'b 'c 'd 'e)
                                 (ideque 'x 'y 'z))))
 (test '((1) (2) (3))
       (ideque->list (ideque-zip (ideque 1 2 3))))
 (test '()
       (ideque->list (ideque-zip (ideque 1 2 3) (ideque))))
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
 (test 3 (ideque-any (lambda (x) (and (number? x) x))
                     (ideque 'a 3 'b 'c 4 'd 'e)))
 (test 5 (ideque-any (lambda (x) (and (number? x) x))
                     (ideque 'a 'b 'c 'd 'e 5)))
 (test #f (ideque-any (lambda (x) (and (number? x) x))
                      (ideque 'a 'b 'c 'd 'e)))
 (test 9 (ideque-every (lambda (x) (and (number? x) x))
                       (ideque 1 5 3 2 9)))
 (test #f (ideque-every (lambda (x) (and (number? x) x))
                        (ideque 1 5 'a 2 9)))
 ;; check if we won't see further once we found the result
 (test 1 (ideque-any (lambda (x) (and (odd? x) x))
                     (ideque 2 1 'a 'b 'c 'd)))
 (test #f (ideque-every (lambda (x) (and (odd? x) x))
                        (ideque 1 2 'a 'b 'c 'd)))
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
