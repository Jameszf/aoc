;;; -*- lexical-binding: t; -*-


(defun read-file-as-lines (filename)
  "Reads and returns the lines of a file in a list. 
Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))


(defun parse-line (line)
  (let* ((parts (split-string line))
	 (speed (string-to-number (elt parts 3)))
	 (flyingTime (string-to-number (elt parts 6)))
	 (restingTime (string-to-number (elt parts 13))))
    (list speed flyingTime restingTime)))


(defun calc-distance (reindeer time)
  (let* ((speed (car reindeer))
	(flightTime (cadr reindeer))
	(restTime (car (last reindeer)))
	(travelled (* speed flightTime (/ time (+ flightTime restTime)))))
    (if (> (% time (+ flightTime restTime)) flightTime)
	(+ (* speed flightTime) travelled)
      (+ (* (% time (+ flightTime restTime)) speed) travelled))))


(defun calc-max-distance (reindeers time)
  (seq-max (mapcar (lambda (reindeer)
		     (calc-distance reindeer time))
		   reindeers)))


(defun partOne ()
  (let* ((lines (read-file-as-lines "14.txt"))
	(reindeers (mapcar 'parse-line lines))
	(testTime 2503))
    (message "%s" (calc-max-distance reindeers testTime))))


(defun simulate (reindeers totalTime)
  (let ((scores (mapcar (lambda (x)
			  (list x 0))
			reindeers)))
    (dotimes (time (+ totalTime 1))
      (unless (= time 0)
	(let ((bestDist (calc-max-distance reindeers time)))
	  (setq scores (mapcar (lambda (x)
				 (let ((reindeer (car x))
				       (points (cadr x)))
				   (if (= (calc-distance reindeer time) bestDist)
				       (list reindeer (+ points 1))
				     x)))
			       scores)))))
    scores))


(defun partTwo ()
  (let* ((lines (read-file-as-lines "14.txt"))
	 (reindeers (mapcar 'parse-line lines))
	 (testTime 2503)
	 (scores (simulate reindeers testTime)))
    (message "%d" (seq-max (mapcar (lambda (x) (cadr x)) scores)))))
  

;; (partOne)
(partTwo)
