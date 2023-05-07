
(defun read-file-as-lines (filename)
  "Reads and returns the lines of a file in a list. Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents "2.txt")
    (split-string (buffer-string) "\n" t)))


(defun calcBoxArea (boxStr)
  "Calculates the wrapping paper required for a box.
   boxStr (string): Unparsed string of box dimensions as given in input."
  (let ((dims (mapcar (lambda (n) (string-to-number n)) (split-string boxStr "x" t))))
    (setq d1 (* (car dims) (cadr dims)))
    (setq d2 (* (car dims) (car (last dims))))
    (setq d3 (* (cadr dims) (car (last dims))))
    (+ (* 2 d1) (* 2 d2) (* 2 d3) (min d1 d2 d3))))


(defun partOne ()
  "Solution to part 1 of Day 2 for AOC2015."
  (let ((boxDims (read-file-as-lines "2.txt"))
	(ans 0))
    (dolist (box boxDims)
      (setq ans (+ ans (calcBoxArea box))))
    (message (number-to-string ans))))


(defun minTwo (dims)
  "Returns the two minimum dimensions of a box as a list."
  (let ((largest (seq-max dims)))
    (cond ((= largest (car dims)) (cdr dims))
	  ((= largest (cadr dims)) (list (car dims) (car (last dims))))
	  (t (butlast dims 1)))))


(defun calcVolume (dims)
  "Calculates the volume of a box."
  (* (car dims) (cadr dims) (car (last dims))))


(defun calcRibbon (boxStr)
  "Calculates the ribbon necessary for a single box."
  (let ((dims (mapcar (lambda (n) (string-to-number n)) (split-string boxStr "x" t))))
    (+ (* 2 (seq-reduce '+ (minTwo dims) 0)) (calcVolume dims))))


(defun partTwo ()
  "Solution to part 2 of Day 2 of AOC2015."
  (let ((boxDims (read-file-as-lines "2.txt"))
	(ans 0))
    (dolist (box boxDims)
      (setq ans (+ ans (calcRibbon box))))
    (message (number-to-string ans))))


(partTwo)
