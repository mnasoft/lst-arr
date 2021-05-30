;;;; package.lisp

(defpackage #:lst-arr
  (:use #:cl)
  (:export transpose calc-min-list-list-size
           list-list-transponate
           items-by-keys
           array2d->list-list-by-col
           item-by-key
           array2d->list-list-by-row
           print-list-list
           list-list->array
           skip-n-items
           arr-to-list make-array2d-from-list
           list->2d-list-left-right
           list2d->array2d
           calc-max-list-list-size
           list->2d-list-down-top
           print-array2d))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
