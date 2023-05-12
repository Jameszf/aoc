

(defun read-file-as-lines (filename)
  "Reads and returns the lines of a file in a list. Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))


(defun contains-dchar (str dchar)
  (let ((ans nil))
    (dotimes (idx (- (length str) 1))
      (setq currd (string (elt str idx) (elt str (+ idx 1))))
      (setq ans (or ans (string= currd dchar))))
    (progn ans)))


(defun is-vowel (char)
  (let ((vowels "aeoui"))
    (seq-some (lambda (vowel) (equal vowel char)) vowels)))


(defun contains-vowels (str)
  (let ((vowels "aeoui"))
    (>= (seq-reduce (lambda (acc char) (if (is-vowel char)
					   (+ acc 1)
					 (progn acc))) str 0) 3)))


(defun contains-consec (str)
  (let ((alphabet "abcdefghijklmnopqrstuvwxyz"))
    (seq-reduce (lambda (acc x) (or acc (contains-dchar str (string x x)))) alphabet nil)))


(defun contains-disallowed (str)
  (let ((disallowed-dchars '("ab" "cd" "pq" "xy")))
	(seq-reduce (lambda (acc x) (or (contains-dchar str x) acc)) disallowed-dchars nil)))


(defun is-nice (str)
    (and (contains-vowels str)
	 (contains-consec str)
	 (not (contains-disallowed str))))


(defun partOne ()
  (let ((lines (read-file-as-lines "5.txt"))
	(ans 0))
    (dolist (line lines)
      (when (is-nice line)
	(setq ans (+ ans 1))))
    (message (number-to-string ans))))


(defun two-repeats (pair rest)
  "Used for checking if there are two, non-overlapping, consecutive characters in a string."
  (cond ((< (length rest) 2) nil)
	((contains-dchar rest pair) t)
	(t (two-repeats (string (elt pair 1) (elt rest 0)) (substring rest 1 nil)))))


(defun contains-triple (char str)
  "Checks if x_x, where x is a letter exists in str."
  (cond ((< (length str) 2) nil)
	((equal char (elt str 1)) t)
	(t (contains-triple (elt str 0) (substring str 1 nil)))))


(defun partTwo ()
  (let ((lines (read-file-as-lines "5.txt"))
	(ans 0))
    (dolist (line lines)
      (when (and (two-repeats (substring line 0 2) (substring line 2 nil))
		 (contains-triple (elt line 0) (substring line 1 nil)))
	(setq ans (+ ans 1))))
    ans))


(message (number-to-string (partOne)))
(message (number-to-string (partTwo)))
