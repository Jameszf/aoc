

(defun read-file-as-lines (filename)
  "Reads and returns the lines of a file in a list. Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))


(defun -find-backslashes (line idx)
  (when idx
    (cons (string (elt line idx) (elt line (+ idx 1)))
	  (-find-backslashes line (string-match "\\\\." line (+ idx 2))))))


(defun find-backslashes (line)
  (-find-backslashes line (string-match "\\\\." line)))


(defun count-encoded (line)
  (seq-reduce (lambda (acc x) (if (or (equal x ?\") (equal x ?\\))
				  (+ acc 1)
				acc))
	      line 0))


(defun calc-diff (line)
  (let* ((backslashes (find-backslashes line))
	(codeCharNum (length line))
	(memCharNum (- codeCharNum 2 (seq-reduce
				      (lambda (acc x)
					(if (string= x "\\x")
					    (+ acc 3)
					  (+ acc 1)))
				      backslashes 0))))
    (- codeCharNum memCharNum)))


(defun partOne ()
  (let* ((lines (read-file-as-lines "8.txt"))
	 (ans (seq-reduce (lambda (acc x) (progn
					    (message (number-to-string (calc-diff x)))
					    (+ acc (calc-diff x))))
			  lines 0)))
    (message (number-to-string ans))))


(defun partTwo ()
  (let* ((lines (read-file-as-lines "8.txt"))
	 ;; length of encoded string = length of string + number of encoded characters + 2
	 ;; If subtracting the length of string then formula is just summation of encoded
	 ;; characters + 2 * number of lines.
	 (ans (seq-reduce (lambda (acc x) (+ acc (count-encoded x) 2))
			  lines 0)))
    (message (number-to-string ans))))


(partOne)
(partTwo)
