(module (srfi 134)
  (ideque ideque-tabulate ideque-unfold ideque-unfold-right
   ideque? ideque-empty? ideque= ideque-any ideque-every

   ideque-front ideque-add-front ideque-remove-front
   ideque-back  ideque-add-back  ideque-remove-back

   ideque-ref
   ideque-take ideque-take-right ideque-drop ideque-drop-right
   ideque-split-at

   ideque-length ideque-append ideque-reverse
   ideque-count ideque-zip

   ideque-map ideque-filter-map
   ideque-for-each ideque-for-each-right
   ideque-fold ideque-fold-right
   ideque-append-map

   ideque-filter ideque-remove ideque-partition

   ideque-find ideque-find-right
   ideque-take-while ideque-take-while-right
   ideque-drop-while ideque-drop-while-right
   ideque-span ideque-break

   list->ideque ideque->list
   generator->ideque ideque->generator
   )

  (import (scheme)
          (except (chicken base) define-record define-record-type)
          (chicken condition)
          (chicken type)
          (except (srfi 1) assoc member)
          (srfi 41)
          (srfi 134 internal)
          (typed-records))

  ;; SRFI 158 shim
  (define (generator->list gen)
    (letrec
     ((build
       (lambda (xs)
         (let ((x (gen)))
           (if (eof-object? x)
               (reverse xs)
               (build (cons x xs)))))))
      (build '())))

  (define-type ideque (struct <ideque>))

  (include "exceptions.scm")
  (include "ideque-streams.scm"))
