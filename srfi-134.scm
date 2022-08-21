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
          (chicken base)
          (chicken condition)
          (except (chicken type) assume)
          (srfi 1)
          (only (srfi 158) generator->list)
          (typed-records))

  (define-syntax assert-type
    (syntax-rules ()
      ((assert-type loc expr)
       (unless expr
         (abort
          (make-composite-condition
           (make-property-condition 'exn
            'location loc
            'message "type check failed"
            'arguments (list 'expr))
           (make-property-condition 'type)
           (make-property-condition 'assertion)))))))

  (define (arity-exception loc argl)
    (abort
     (make-composite-condition
      (make-property-condition 'exn
       'location loc
       'message "invalid number of arguments"
       'arguments argl)
      (make-property-condition 'arity)
      (make-property-condition 'assertion))))

  (define (bounds-exception loc msg . args)
    (abort
     (make-composite-condition
      (make-property-condition 'exn
       'location loc
       'message msg
       'arguments args)
      (make-property-condition 'bounds)
      (make-property-condition 'assertion))))

  (include "ideque-impl.scm"))
