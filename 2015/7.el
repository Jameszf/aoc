

(defun read-file-as-lines (filename)
  "Reads and returns the lines of a file in a list. Empty lines are filtered out."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))


(defun string-is-number-p (str)
  (string-match-p "^[0-9]+$" str))


(defun parse-instruct (line)
  (let* ((connection (split-string line " -> "))
	 (inputs (split-string (car connection) " "))
	 (ops (mapcar (lambda (x) (if (string-is-number-p x)
				      (string-to-number x)
				    x))
		      inputs)))
    (list (cadr connection) ops)))


(defun eval-gate (op arg1 &optional arg2)
  (cond ((string= op "RSHIFT") (lsh arg1 (- arg2)))
	((string= op "LSHIFT") (lsh arg1 arg2))
	((string= op "OR") (logior arg1 arg2))
	((string= op "AND") (logand arg1 arg2))
	((string= op "NOT") (lognot arg1))))


(defun find-wire (wire table memory)
  (if (gethash wire memory nil)
      (gethash wire memory)
    (let* ((inputs (cadr (assoc wire table))))
      (if (numberp wire)
	  wire
	(cond ((= (length inputs) 1) (puthash wire (find-wire (car inputs) table memory) memory))
	      ((= (length inputs) 2) (puthash wire (eval-gate "NOT" (find-wire (cadr inputs) table memory)) memory))
	      (t (puthash wire (eval-gate
				(cadr inputs)
				(find-wire (car inputs) table memory)
				(find-wire (elt inputs 2) table memory))
			  memory)))))))


(defun partOne ()
  (let* ((lines (read-file-as-lines "7.txt"))
	 (table (mapcar 'parse-instruct lines)))
    (message (number-to-string (find-wire "a" table (make-hash-table))))))


(defun partTwo ()
  "Manually edit the initial signal value that goes into wire b in the input text file."
  nil)

(partOne)

