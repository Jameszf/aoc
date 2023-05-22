;;; -*- lexical-binding: t; -*-


(defun read-file (filename)
  "Reads and returns the lines of a file in a list. Empty lines are filtered 
out."
  (with-temp-buffer
    (insert-file-contents filename)
    (replace-string " " "")
    (buffer-string)))


(defun is-digit-p (char)
  (or (= char ?1)
      (= char ?2)
      (= char ?3)
      (= char ?4)
      (= char ?5)
      (= char ?6)
      (= char ?7)
      (= char ?8)
      (= char ?9)
      (= char ?0)
      (= char ?-)))


(defun get-next-number (str start)
  (let* ((startNum (string-match "-?[0-9]+" str start))
	(endNum startNum))
    (when startNum 
      (while (is-digit-p (elt str endNum))
	(setq endNum (+ endNum 1)))
      (list startNum endNum))))


(defun partOne ()
  (let* ((json (read-file "12.txt"))
	(idx (get-next-number json 0))
	(ans 0))
    (while idx
      (setq ans (+ ans (string-to-number
			(substring json
				   (car idx)
				   (cadr idx)))))
      (setq idx (get-next-number json (cadr idx))))
    (message "%d" ans)))


(defun has-red-p (object)
  (seq-some (lambda (x) (equal (cdr x) "red")) object))


(defun just-array-p (obj)
  (and (arrayp obj) (not (stringp obj))))


(defun process-element (acc x)
  (cond ((numberp x) (+ acc x))
	((nested-alist-p x) (+ acc (count-object x)))
	((just-array-p x) (+ acc (count-array x)))
	(t acc)))


(defun count-array (array)
  (seq-reduce 'process-element array 0))


(defun count-object (object)
  (if (has-red-p object)
      0
    (seq-reduce (lambda (acc x)
		  (cond ((numberp (cdr x)) (+ acc (cdr x)))
			((just-array-p (cdr x)) (+ acc (count-array (cdr x))))
			((nested-alist-p (cdr x)) (+ acc (count-object (cdr x))))
			(t acc)))
		object 0)))


(defun count-json (json)
  (if (just-array-p json)
      (count-array json)
    (count-object json)))


(defun partTwo ()
  (let* ((json-str (read-file "12.txt"))
	 (json (json-parse-string json-str :object-type 'alist)))
    (message "%d" (count-json json))))


(partOne)
(partTwo)


