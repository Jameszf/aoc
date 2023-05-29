;;; -*- lexical-binding: t; -*-


(defun read-file-as-lines (filename)
  "Reads and returns the lines of a file in a list. 
Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))


(defun zip (list1 list2)
  (let ((newList '()))
    (dotimes (idx (length list1))
      (setq newList (cons (list (elt list1 idx) (elt list2 idx)) newList)))
    (reverse newList)))


(defun remove-extra-char (str)
  (if (or (= (elt str (- (length str) 1)) ?:)
	  (= (elt str (- (length str) 1)) ?,))
      (substring str 0 -1)
    str))


(defun parse-line (line)
  (let* ((parts (mapcar 'remove-extra-char (split-string line))))
    (list (list (elt parts 2) (string-to-number (elt parts 3)))
	  (list (elt parts 4) (string-to-number (elt parts 5)))
	  (list (elt parts 6) (string-to-number (elt parts 7))))))



(defun matching-sue (lookFor sue)
  (seq-reduce (lambda (acc x)
		(if (or (not (cadr (assoc (car x) sue)))
			(apply (car (last x))
			       (cadr (assoc (car x) sue))
			       (list (nth 1 x))))
		    acc
		  nil))
	  lookFor t))


(defun partOne ()
  (let* ((lines (read-file-as-lines "16.txt"))
	 (aunts (zip (number-sequence 1 500) (mapcar 'parse-line lines)))
	 (giftingSue (list '("children" 3 =)
			   '("cats" 7 =)
			   '("samoyeds" 2 =)
			   '("pomeranians" 3 =)
			   '("akitas" 0 =)
			   '("vizslas" 0 =)
			   '("goldfish" 5 =)
			   '("trees" 3 =)
			   '("cars" 2 =)
			   '("perfumes" 1 =)))
	 (ans (seq-filter (lambda (x)
			    (matching-sue giftingSue (cadr x)))
			  aunts)))
    (message "%s" ans)))


(defun partTwo ()
  (let* ((lines (read-file-as-lines "16.txt"))
	 (aunts (zip (number-sequence 1 500) (mapcar 'parse-line lines)))
	 (giftingSue (list '("children" 3 =)
			   '("cats" 7 >)
			   '("samoyeds" 2 =)
			   '("pomeranians" 3 <)
			   '("akitas" 0 =)
			   '("vizslas" 0 =)
			   '("goldfish" 5 <)
			   '("trees" 3 >)
			   '("cars" 2 =)
			   '("perfumes" 1 =)))
	 (ans (seq-filter (lambda (x)
			    (matching-sue giftingSue (cadr x)))
			  aunts)))
    (message "%s" ans)))


(partOne)
