;;;; lst-arr.lisp

(in-package #:lst-arr)

(let ((arr (make-array '(3 4) :initial-contents '((1 2 3 4) (5 6 7 8) (9 10 11 12)))))
  (format t "arr~%")
  (print-array2d arr )
  (format t "(array2d->list-list-by-row arr)~%")
  (print-list-list (array2d->list-list-by-row arr))
  (format t "(array2d->list-list-by-col arr)~%")
  (print-list-list (array2d->list-list-by-col arr))
  )

(let ((lst '((1) (2 3) (4 5 6 ))))
  (format t "lst~%")
  (print-list-list lst)
  (format t "~%(list-list->array lst)~%")
  (print-array2d (list-list->array lst))
  (format t "~%(list-list-transponate lst)~%")
  (print-list-list (list-list-transponate lst))
  )
