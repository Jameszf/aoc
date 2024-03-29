
(defun read-file (filename)
  "Reads and returns the lines of a file in a list. Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))


(defun in-list (item list)
  "Checks if an item is in a list."
  (seq-reduce (lambda (acc x) (or acc (equal x item))) list nil))


(defun partOne ()
  (let ((input (read-file "3.txt"))
	(x 0)
	(y 0)
	(visited (cons (list 0 0) nil)))
    (mapcar (lambda (c)
	      (cond ((= c ?^) (setq y (+ y 1)))
		    ((= c ?v) (setq y (- y 1)))
		    ((= c ?<) (setq x (- x 1)))
		    ((= c ?>) (setq x (+ x 1))))
	      (when (not (in-list (list x y) visited))
		(setq visited (cons (list x y) visited)))) input)
    (message (number-to-string (length visited)))))


(defun partTwo ()
  (let ((input (read-file "3.txt"))
	(x1 0)
	(y1 0)
	(x2 0)
	(y2 0)
	(isSanta t)
	(visited '((nil 0 0) (t 0 0))))
    (mapcar (lambda (c)
	      (cond ((= c ?^) (setq y1 (+ y1 1)))
		    ((= c ?v) (setq y1 (- y1 1)))
		    ((= c ?<) (setq x1 (- x1 1)))
		    ((= c ?>) (setq x1 (+ x1 1))))
	      (when (not (in-list (list isSanta x1 y1) visited))
		(setq visited (cons (list isSanta x1 y1) visited)))
	      ;; Swap coordinates.
	      (setq x0 x1)
	      (setq y0 y1)
	      (setq x1 x2)
	      (setq y1 y2)
	      (setq x2 x0)
	      (setq y2 y0)
	      ;; Switch between santa and robot.
	      (xor isSanta t)) input)
    (message (number-to-string (- (length visited) 1)))))

(partOne)
(partTwo)
