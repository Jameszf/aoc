
(defun good-hash (hash)
  (string= (substring hash 0 5) "00000"))


(defun partOne ()
  (let ((number 1)
	(secret "yzbqklnj"))
    (while (not (good-hash (md5 (concat secret (number-to-string number)))))
      (setq number (+ number 1)))
    (message (number-to-string number))))


(defun good-hash-2 (hash)
  (string= (substring hash 0 6) "000000"))


(defun partTwo ()
  (let ((number 1)
	(secret "yzbqklnj"))
    (while (not (good-hash-2 (md5 (concat secret (number-to-string number)))))
      (setq number (+ number 1)))
    (message (number-to-string number))))


(partOne)
(partTwo)
