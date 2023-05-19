
(defun read-file-as-lines (filename)
  "Reads and returns the lines of a file in a list. Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))


(defun parse-edge (line)
  (let* ((parts (split-string line))
	 (v1 (car parts))
	 (v2 (elt parts 2))
	 (cost (string-to-number (car (last parts)))))
    (list v1 v2 cost)))


(defun construct-table (table edge)
  (let* ((v1 (car edge))
	 (v2 (cadr edge))
	 (cost (car (last edge)))
	 (entry1 (gethash v1 table))
	 (entry2 (gethash v2 table)))
    (puthash v1 (cons (list v2 cost) entry1) table)
    (puthash v2 (cons (list v1 cost) entry2) table)
    table))


(defun remove-elem (list elt)
  (cond ((= (length list) 0) '())
	((equal (car list) elt) (remove-elem (cdr list) elt))
	(t (cons (car list) (remove-elem (cdr list) elt)))))


(defun search (table nonvisited current partial func)
  (if (> (length nonvisited) 0)
      (apply func (list
		   (mapcar (lambda (x)
			     (let* ((edges (gethash current table))
				    (edge (assoc x edges))
				    (cost (cadr edge)))
			       (search table (remove-elem nonvisited x) x (+ partial cost) func)))
			   nonvisited)))
    partial))


(defun get-keys (table)
  (let ((keys '()))
    (maphash (lambda (key val) (setq keys (cons key keys))) table)
    keys))


(defun partOne ()
  (let* ((lines (read-file-as-lines "9.txt"))
	 (edges (mapcar 'parse-edge lines))
	 (table (seq-reduce 'construct-table edges (make-hash-table :test 'equal)))
	 (keys (get-keys table))
	 (bestRoutes (mapcar (lambda (x)
			       (search table (remove-elem keys x) x 0 'seq-min))
			     keys)))
    (message (number-to-string (seq-min bestRoutes)))))


(defun partTwo ()
  (let* ((lines (read-file-as-lines "9.txt"))
	 (edges (mapcar 'parse-edge lines))
	 (table (seq-reduce 'construct-table edges (make-hash-table :test 'equal)))
	 (keys (get-keys table))
	 (bestRoutes (mapcar (lambda (x)
			       (search table (remove-elem keys x) x 0 'seq-max))
			     keys)))
    (message (number-to-string (seq-max bestRoutes)))))

;; (partOne)
(partTwo)

