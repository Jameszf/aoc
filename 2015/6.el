
(defun read-file-as-lines (filename)
  "Reads and returns the lines of a file in a list. Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))


(defun parse-coords (c1 c2)
  (let ((parts (append (split-string c1 ",") (split-string c2 ","))))
    (mapcar 'string-to-number parts)))


(defun point-rect-collision (point rect)
  (and (<= (elt rect 0) (car point)) (<= (car point) (elt rect 2))
       (<= (elt rect 1) (cadr point)) (<= (cadr point) (elt rect 3))))


(defun filter-instructs (point instructions)
  (seq-filter (lambda (x) (point-rect-collision point (cadr x))) instructions))


(defun simul-instructs (instructions)
  (seq-reduce (lambda (acc x) (cond ((string= (car x) "on") t)
				    ((string= (car x) "off") nil)
				    (t (xor acc t))))
	      instructions nil))


(defun parse-instructs (lines)
  (reverse
   (seq-reduce (lambda (acc x)
		 (if (string= (car x) "turn")
		     (cons (list (cadr x) (parse-coords (elt x 2) (elt x 4))) acc)
		   (cons (list "t" (parse-coords (elt x 1) (elt x 3))) acc)))
	       lines '())))


;; Original solution (very slow).
(defun partOne ()
  (let* ((lines (mapcar 'split-string (read-file-as-lines "6.txt")))
	(instructions (parse-instructs lines))
	(count 0))
    (dotimes (x 1000)
      (dotimes (y 1000)
	(when (simul-instructs (filter-instructs (list x y) instructions))
	  (setq count (+ count 1)))))
    (message (number-to-string count))))


(defun count (grid func)
  (seq-reduce (lambda (ans row)
		(+ ans (seq-reduce (lambda (acc x)
				     (+ acc (apply func (list x))))
				   row 0)))
	      grid 0))


(defun range-apply (grid range func)
  (let ((x1 (elt range 0))
	(y1 (elt range 1))
	(x2 (elt range 2))
	(y2 (elt range 3)))
    (dotimes (x (- x2 x1 -1))
      (dotimes (y (- y2 y1 -1))
	(let ((point (aref (aref grid (+ x1 x)) (+ y1 y))))
	  (aset (aref grid (+ x1 x)) (+ y1 y) (apply func (list point))))))))


(defun generate-grid (size init)
  (let ((grid (make-vector size 0)))
    (dotimes (idx size)
      (aset grid idx (make-vector size init)))
    grid))


;; Faster part one solution.
(defun partOneB ()
  (let* ((lines (mapcar 'split-string (read-file-as-lines "6.txt")))
	 (instructions (parse-instructs lines))
	 (grid (generate-grid 1000 nil)))
    (mapcar (lambda (x)
	      (cond ((string= (car x) "on")
		     (range-apply grid (cadr x) (lambda (x) t)))
		    ((string= (car x) "off")
		     (range-apply grid (cadr x) (lambda (x) nil)))
		    (t (range-apply grid (cadr x) (lambda (x) (xor x t))))))
	    instructions)
    ;; (message grid)
    (message (number-to-string (count grid (lambda (x) (if x 1 0)))))))


(defun partTwo ()
  (let* ((lines (mapcar 'split-string (read-file-as-lines "6.txt")))
	 (instructions (parse-instructs lines))
	 (grid (generate-grid 1000 0)))
    (mapcar (lambda (x)
	      (cond ((string= (car x) "on")
		     (range-apply grid (cadr x) (lambda (x) (+ x 1))))
		    ((string= (car x) "off")
		     (range-apply grid (cadr x) (lambda (x) (max 0 (- x 1)))))
		    (t (range-apply grid (cadr x) (lambda (x) (+ x 2))))))
	    instructions)
    (message (number-to-string (count grid (lambda (x) x))))))


(partOne)
(partOneB)
(partTwo)
