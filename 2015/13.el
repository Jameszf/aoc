;;; -*- lexical-binding: t; -*-


(defun read-file-as-lines (filename)
  "Reads and returns the lines of a file in a list. 
Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))


(defun remove-elem (list elt)
  "Removes a single element from a list. Uses equal to do comparisons."
  (cond ((= (length list) 0) '())
	((equal (car list) elt) (remove-elem (cdr list) elt))
	(t (cons (car list) (remove-elem (cdr list) elt)))))


(defun get-keys (table)
  (let ((keys '()))
    (maphash (lambda (key value)
	       (setq keys (cons key keys)))
	     table)
    keys))


(defun parse-input-line (line)
  (let* ((parts (split-string line " "))
	 (change (string-to-number (elt parts 3)))
	 (delta (if (string= (elt parts 2) "gain")
		    change
		  (- change))))
    (list (elt parts 0) delta (substring (elt parts 10) 0 -1))))


(defun add-relation (table relation)
  (let* ((person1 (car relation))
	 (delta (cadr relation))
	 (person2 (car (last relation)))
	 (current-relations (gethash person1 table)))
    (puthash person1 (cons (list person2 delta) current-relations) table))
  table)


(defun build-hash-table (relations)
  (seq-reduce 'add-relation relations (make-hash-table :test 'equal)))


(defun calc-happiness (table arrangement)
  (if (<= (length arrangement) 1)
      0
    (let* ((p1 (car arrangement))
	   (p2 (cadr arrangement))
	   (relations1 (gethash p1 table))
	   (relations2 (gethash p2 table)))
      (+ (cadr (assoc p2 relations1))
	 (cadr (assoc p1 relations2))
	 (calc-happiness table (cdr arrangement))))))


(defun generate-permutations (people)
  (let ((perms '()))
    (if (= (length people) 0)
	nil
      (dolist (person people)
	(let ((subperms (generate-permutations (remove-elem people person))))
	  (if subperms
	      (dolist (perm subperms)
		(setq perms (cons (cons person perm) perms)))
	    (setq perms (cons (list person) perms))))))
    perms))


(defun partOne ()
  (let* ((lines (read-file-as-lines "13.txt"))
	 (relations (mapcar 'parse-input-line lines))
	 (table (build-hash-table relations))
	 (permutations (generate-permutations (get-keys table)))
	 (ans (seq-max (mapcar (lambda (perm)
				 (calc-happiness table (cons (car (last perm)) perm)))
			       permutations))))
    (message "%d" ans)))


(defun add-neutral (table)
  (maphash (lambda (key value)
	     (puthash key (cons (list "Neutral" 0) value) table)
	     (puthash "Neutral" (cons (list "Neutral" 0) value) table))
	   table)
  (puthash "Neutral" (mapcar (lambda (person)
			       (list person 0))
			     (get-keys table))
	   table))


(defun calc-answer (table perms)
  (seq-max (mapcar (lambda (perm)
		     (calc-happiness table (cons (car (last perm)) perm)))
		   perms)))


;;; Slow (no DP) but able to run in a reasonable amount of time.
(defun partTwo ()
  (let* ((lines (read-file-as-lines "13.txt"))
	 (relations (mapcar 'parse-input-line lines))
	 (table (build-hash-table relations))
	 (perms (generate-permutations (cons "Neutral" (get-keys table)))))
    (add-neutral table)
    (message "%d" (calc-answer table perms))))


;; (partOne)
(partTwo)

