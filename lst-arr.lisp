;;;; lst-arr.lisp

(in-package #:lst-arr)

;;; "lst-arr" goes here. Hacks and glory await!

(defun calc-max-list-list-size(l)
  "Выполняет поиск максимального количества элементов в списке списков.
Возвращает список первым элементом которого является количество элементов списке,
а вторым элементом - максимальное количество элементов в одном из элементов списка.
Пример использования:
(calc-max-list-list-size '((1 2)(2)(1 2 5)(1 2)))
=> (4 3)
(calc-max-list-list-size '(()()()()))
=> (4 0)
(calc-max-list-list-size '((1)(1 2 3 4 5 6)))
=> (2 6)
"
  (let (
	(i (length l))
	(j (apply #'max (mapcar #'length l)))
	)
    (list i j)))

(defun calc-min-list-list-size(l)
  "Выполняет поиск минимального количества элементов в списке списков.
Возвращает список первым элементом которого является количество элементов списке,
а вторым элементом - максимальное количество элементов в одном из элементов списка.
Пример использования:
(calc-min-list-list-size '((1 2)(2)(1 2 5)(1 2)))
=> (4 1)
(calc-min-list-list-size '(()()()()))
=> (4 0)
(calc-min-list-list-size '((1 2)(1 2 3 4 5 6)))
=> (2 2)
"
  (let (
	(i (length l))
	(j (apply #'min (mapcar #'length l)))
	)
    (list i j)))

(defun list-list->array(l)
  "Создает матрицу типа #2A, состоящую из элементов списка списков.
Если в некоторых подсписках элементов меньше чем в других - 
они заменяются 0.0
Пример использования:
(list-list->array '((1 2)(2)(1 2 5)(1 2)))
=> #2A((1 2 0.0) (2 0.0 0.0) (1 2 5) (1 2 0.0))
"
  (let* ((sz (calc-max-list-list-size l))
	 (i 0)
	 (j 0)
	 (a (make-array sz :initial-element 0.0)))
    (mapc #'(lambda (r)
	      (mapc #'(lambda(c)
			(setf (aref a i j) c)
			(incf j) )
		    r)
	      (setf j 0)
	      (incf i))
	  l)
    a))

(defun array2d->list-list-by-col(a)
  (do* ( (in (array-dimension a 0))
	 (jn (array-dimension a 1))
	 (j 0 (1+ j))
	 (lst nil)
	 )
       ( (>= j jn) (reverse lst))
    (setf lst (cons 
	       (do ( (col nil)
		     (i 0 (1+ i)))
		   ( (>= i in) (reverse col))
		 (setf col (cons (aref a i j) col)))
	       lst))))




(defun list-list-transponate(l)
  "Транспонирование списка-списков
;;(list-list-transponate '((1 2) (2 3) (3 4) (4 5) (5 6)))
=> ((1 2 3 4 5) (2 3 4 5 6))
"
  (array2d->list-list-by-col (list-list->array l)))

(export 'array2d->list-list-by-row)

(defun array2d->list-list-by-row (a)
  (list-list-transponate (array2d->list-list-by-col a)))
