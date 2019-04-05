;;;; package.lisp

(defpackage #:lst-arr)

(defpackage #:lst-arr
  (:use #:cl)
  (:export lst-arr::calc-max-list-list-size
	   lst-arr::calc-min-list-list-size
	   )
  (:export lst-arr::array2d->list-list-by-col
	   lst-arr::array2d->list-list-by-row
	   )
  (:export lst-arr::list-list->array
	   lst-arr::list-list-transponate
	   lst-arr::print-list-list
	   lst-arr::print-array2d
	   lst-arr::make-array2d-from-list
	   )
  (:export lst-arr::transpose
	   lst-arr::list->2d-list-left-right
	   lst-arr::list->2d-list-down-top
	   )
  (:export lst-arr::arr-to-list
	   )
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
