;;; -*- lexical-binding: t; -*-


(defun read-file-as-lines (filename)
  "Reads and returns the lines of a file in a list. 
Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))


(defun remove-comma (numStr)
  (if (= (elt numStr (- (length numStr) 1)) ?,)
      (substring numStr 0 -1)
    numStr))


(defun extract-indices (indices seq)
  (mapcar (lambda (idx)
	    (elt seq idx))
	  indices))


(defun parse-line (line)
  (let* ((parts (mapcar 'remove-comma (split-string line)))
	 (numStrs (extract-indices '(2 4 6 8) parts)))
    (mapcar 'string-to-number numStrs)))


(defun multi-rows (matrix)
  (seq-map (lambda (row)
	     (seq-map (lambda (y)
			(* y (cadr row)))
		      (car row)))
	   matrix))


(defun sum-cols (matrix)
  (seq-reduce (lambda (acc row)
		(seq-map (lambda (x)
			   (+ (car x) (cadr x)))
			 (zip acc row)))
	      matrix '(0 0 0 0)))


(defun zip (list1 list2)
  (let ((newList '()))
    (dotimes (idx (length list1))
      (setq newList (cons (list (elt list1 idx) (elt list2 idx)) newList)))
    (reverse newList)))


(defun calc-score (ingredients a b c d)
  (let* ((zipped (zip ingredients (list a b c d)))
	 (attrs (multi-rows zipped)))
    (seq-reduce (lambda (acc x)
		  (* acc x))
		(sum-cols attrs) 1)))


(defun search (ingredients mem a b c d)
  ;; (message "Calculating: (%d %d %d %d)..." a b c d)
  (unless (gethash (list a b c d) mem nil)
    (if (= 100 (+ a b c d))
	(puthash (list a b c d) (calc-score ingredients a b c d) mem)
      (progn
	(search ingredients mem (+ 1 a) b c d)
	(search ingredients mem a (+ 1 b) c d)
	(search ingredients mem a b (+ 1 c) d)
	(search ingredients mem a b c (+ 1 d))))))


(defun search2 (ingredients)
  (let ((ans 0))
    (dotimes (a 100)
      (dotimes (b 100)
	(dotimes (c 100)
	  (dotimes (d 100)
	    ;; (message "Calculating: (%d %d %d %d)..." a b c d)
	    (when (= (+ a b c d) 100)
	      (setq ans (max ans (calc-score ingredients a b c d))))))))
    ans))


(defun partOne ()
  (let* ((lines (read-file-as-lines "15.txt"))
	 (ingredients (mapcar 'parse-line lines))
	 (table (search ingredients (make-hash-table :test 'equal) 0 0 0 0)))
    (maphash (lambda (key value)
	       (message "%s %s" key value))
	     table)))


(defun partOne2 ()
  (let* ((lines (read-file-as-lines "15.txt"))
	 (ingredients (mapcar 'parse-line lines)))
    (message "%d" (search2 ingredients))))


(defun calc-score-2 (ingredients amounts)
  (let* ((zipped (zip ingredients amounts))
	 (attrs (multi-rows zipped)))
	 (seq-reduce (lambda (acc x)
		       (if (> x 0)
			   (* acc x)
			 0))
		     (sum-cols attrs) 1)))

(defun increment (idx list1)
  (if (= idx 0)
      (cons (+ (car list1) 1) (cdr list1))
    (cons (car list1) (increment (- idx 1) (cdr list1)))))


(defun seq-sum (seq)
  (seq-reduce (lambda (acc x)
		(+ acc x))
	      seq 0))


(defun search3 (ingredients amounts)
  (if (= 100 (seq-sum amounts))
      amounts
    (let* ((nextAmounts (seq-map (lambda (x)
				   (increment x amounts))
				 (number-sequence 0 (- (length amounts) 1))))
	   (bestNext (seq-reduce (lambda (acc x)
				   (if (>= (calc-score-2 ingredients x)
					  (calc-score-2 ingredients acc))
				       x
				     acc))
				 nextAmounts '(0 0 0 0))))
      (search3 ingredients bestNext))))


(defun partOne3 ()
  (let* ((lines (read-file-as-lines "15.txt"))
	 (ingredients (mapcar 'parse-line lines))
	 (bestAmounts (search3 ingredients '(1 1 1 1))))
    (message "%s %d" bestAmounts (calc-score-2 ingredients bestAmounts))))


(defun get-calories (line)
  (let* ((parts (split-string line)))
    (string-to-number (car (last parts)))))

(defun calc-calories (calories amounts)
  (seq-reduce (lambda (acc x)
		(+ acc (* (car x) (cadr x))))
	      (zip calories amounts) 0))


(defun better-amounts (ingredients calories current new)
  (if (and (= (calc-calories calories new) 500)
	   (> (calc-score-2 ingredients new)
	      (calc-score-2 ingredients current)))
      new
    current))


(defun search4 (ingredients calories amounts)
  (if (= 3 (length amounts))
      (cons (- 100 (seq-sum amounts)) amounts)
    (seq-reduce (lambda (acc x)
		  (better-amounts ingredients calories acc (search4
							    ingredients
							    calories
							    (cons x amounts))))
		(number-sequence 0 (- 100 (seq-sum amounts))) '(0 0 0 0))))


(defun partTwo ()
  (let* ((lines (read-file-as-lines "15.txt"))
	 (ingredients (reverse (mapcar 'parse-line lines)))
	 (calories (reverse (mapcar 'get-calories lines)))
	 (ans (search4 ingredients calories '())))
    (message "%s %d" ans (calc-score-2 ingredients ans))))

;; (partOne3)
;; (partOne2)
;; (message "%d" (calc-score '((-1 -2 6 3) (2 3 -2 -1) (0 0 0 0) (0 0 0 0)) 44 56 0 0))
;; (message "%s" (zip '(1 2 3 4) '(5 6 7 8)))
;; (message "%s" (sum-cols '((112 168 -112 -56) (-44 -88 264 132))))
(partTwo)


