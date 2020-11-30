(define eof-object
  (let ((eof (read (open-input-string ""))))
    (lambda () eof)))
