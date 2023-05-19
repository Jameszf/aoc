
(defun triplep (c1 c2 c3)
  (and (= c1 (+ c2 1)) (= c2 (+ c3 1))))


(defun bad-char-p (char)
  (or (= char ?i) (= char ?o) (= char ?l)))


(defun good-password-p (str)
  (and (>= (count-pairs str) 2)
       (has-triple str)
       (not (seq-some 'bad-char-p str))))


(defun string-to-list (str)
  (mapcar (lambda (x) x) str))


(defun list-to-string (str)
  (mapconcat (lambda (x) (string x)) str ""))


(defun -count-pairs (str char)
  (cond ((= (length str) 0) 0)
	((equal char (car str)) (+ 1 (-count-pairs (cddr str) (cadr str))))
	(t (-count-pairs (cdr str) (car str)))))


(defun count-pairs (str)
  (-count-pairs (cdr str) (car str)))


(defun -has-triple (str char)
  (cond ((< (length str) 3) nil)
	(t (or (triplep char (car str) (cadr str))
	       (-has-triple (cdr str) (car str))))))


(defun has-triple (str)
  (-has-triple (cdr str) (car str)))


(defun increment-password (str)
  (if (= (car str) ?z)
      (cons ?a (increment-password (cdr str)))
    (cons (+ (car str) 1) (cdr str))))


(defun partOne ()
  (let* ((password (string-to-list (reverse "hepxcrrq"))))
    (while (not (good-password-p password))
      (setq password (increment-password password)))
    (message "%s" (list-to-string (reverse password)))))


(defun partTwo ()
  (let* ((password (string-to-list (reverse "hepxcrrq"))))
    (while (not (good-password-p password))
      (setq password (increment-password password)))
    (setq password (increment-password password))
    (while (not (good-password-p password))
      (setq password (increment-password password)))
    (message "%s" (list-to-string (reverse password)))))


;; (partOne)
(partTwo)
