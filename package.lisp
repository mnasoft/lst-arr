;;;; package.lisp

(defpackage #:lst-arr
  (:use #:cl)
  (:export calc-max-list-list-size)
  (:export calc-min-list-list-size)
  (:export list-list->array)
  (:export array2d->list-list-by-col)
  (:export list-list-transponate))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
