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
  (:intern lst-arr::list-list->array
	   lst-arr::arr-to-list
	   )
  (:export lst-arr::list2d->array2d
	   lst-arr::list-list-transponate
	   lst-arr::print-list-list
	   lst-arr::print-array2d
	   lst-arr::make-array2d-from-list
	   )
  (:export lst-arr::transpose
	   lst-arr::list->2d-list-left-right
	   lst-arr::list->2d-list-down-top
	   )
  (:export lst-arr::skip-n-items
	   lst-arr::item-by-key
	   lst-arr::items-by-keys
	   )
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
