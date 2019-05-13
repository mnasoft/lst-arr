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
  (let ((i (length l))
	(j (apply #'max (mapcar #'length l))))
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

(defun list2d->array2d (list2d)
  (list-list->array list2d))

(defun array2d->list-list-by-col(a)
  "Выполняет преобразования 2d массива в список списков заменяя строки столбцами."
  (do* ((in (array-dimension a 0))
	(jn (array-dimension a 1))
	(j 0 (1+ j))
	(lst nil))
       ((>= j jn) (reverse lst))
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

(defun array2d->list-list-by-row (a)
  "Выполняет преобразования 2d массива в список списков."
  (list-list-transponate (array2d->list-list-by-col a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-list-list (lst &key (format-string " (~{~6f~^ ~})~%") (stream t))
  "Выполняет построчный вывод списка-списков.
Пример использования:
 (lst-arr:print-list-list '((1 2 3 4)(5 6 7 8)) :format-string \" (~{~1a~^ ~})~%\")
=>
 (
  (1 2 3 4)
  (5 6 7 8)
 )
 (lst-arr:print-list-list '((1 2 3 4)(5 6 7 8)))
 (
  (   1.0    2.0    3.0    4.0)
  (   5.0    6.0    7.0    8.0)
 )
"
  (format stream "(~%")
  (mapc
   #'(lambda (el) (format stream format-string el))
   lst)
  (format stream ")~%"))

(defun print-array2d (arr &key (format-string "|~{~6f~^ ~}|~%") (stream t))
  "Выполняет построчный вывод масива."
  (mapc
   #'(lambda (el) (format stream format-string el))
   (array2d->list-list-by-row arr)))

(defun make-array2d-from-list (dimensions lst &optional (initial-element 0.0))
  "Создает двумерный массив, состоящий из элементов списка lst.
В массив элементы попадают построчно.
Пример использования:
 (make-array2d-from-list '(2 4) '(1 2 3 4 5))
 =>#2A((1 2 3 4) (5 0.0 0.0 0.0)) "
  (let ((a2d (make-array dimensions :initial-element initial-element)))
    (loop :for i :from 0 :below (first dimensions)
	  :do (loop :for j :from 0 :below (second dimensions)
		    :do (let ((el (nth (+ (* i (second dimensions)) j) lst)))
			  (when el (setf (aref a2d  i j) el)))))
    a2d))

(defun transpose (list)
  "Выполняет транспонирование"
  (apply #'mapcar #'list list))

  
(defun list->2d-list-left-right (rows cols lst)
  "Пример использования:
 (lst-arr::list->2d-list-left-right 2 4 '(1 2 3 4 5 6 7 8)) =>
 ((1 2 3 4) 
  (5 6 7 8))
"
  (let ((rez (lst-arr:array2d->list-list-by-row 
	      (lst-arr:make-array2d-from-list (list rows cols) lst))))
    rez))

(defun list->2d-list-down-top (rows cols lst)
  "Пример использования:
  (lst-arr::list->2d-list-down-top 2 4 '(1 2 3 4 5 6 7 8)) ((2 4 6 8) (1 3 5 7))
  ((2 4 6 8) 
   (1 3 5 7))
"
    (reverse (transpose (list->2d-list-left-right cols rows lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun arr-to-list (ar)
  "Выполняет преобразование двумерного массива в список"
  (labels
      ((get-line (ar n)
	 (let* ((dm (array-dimensions ar)) (cols (second dm)) (row  nil))
	   (dotimes (i cols) (push (aref ar n i) row))
	   (nreverse row))))
    (loop for i below  (first (array-dimensions ar))
       collect (get-line ar i))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (defun skip-n-items (n lst)
    "Пропускает первые n элементов списка lst и возвращает оставшуюся часть списка.
 (skip-n-items -1  '(1 2 3 4 5 6 7 8 9 10))
 (skip-n-items  2  '(1 2 3 4 5 6 7 8 9 10))
 (skip-n-items  9  '(1 2 3 4 5 6 7 8 9 10))
 (skip-n-items 10  '(1 2 3 4 5 6 7 8 9 10))
"
  (let ((rez lst))
    (dotimes (i n rez) (setf rez (cdr rez)))))

(defun item-by-key (key key-lst value-lst)
    "Возвращает "
    (nth (position key key-lst :test #'string=) value-lst))


(defun items-by-keys (key-from key-to key-list value-list &key (test #'string=))
    "Возвращает список значений, содержащихся в value-list, 
  которые находятся в дапазоне позиций от key-from до key-to каких как они 
  встречаются в списке key-list.
     Пример использования:
   (items-by-keys \"C\" \"E\"  
     '(\"A\" \"B\" \"C\" \"D\" \"E\" \"F\" \"G\" \"H\" \"I\") 
     '( 1   2   3   4   5   6   7   8   9 10 11 12))
  "
    (loop
       :for i
       :from (position key-from  key-list :test test)
       :to (position key-to key-list :test test) :collect
	 (nth i value-list)))
