
;; Problem Description: https://adventofcode.com/2015/day/1
;; Date Completed: 03/05/2023


;; Taken from https://www.emacswiki.org/emacs/ElispCookbook
(defun file-string (file)
  "Read the contents of a file and return as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))


(defun partOne ()
  (message "Part One")
  (let ((input (file-string "1.txt"))
	(floor 0)
	(idx 0))
    (while (< idx (length input))
      (if (equal (aref input idx) 40) (setq floor (+ floor 1)) (setq floor (- floor 1)))
      (setq idx (+ idx 1)))
    (message (number-to-string floor))))


(defun partTwo ()
  (message "Part Two")
  (let ((input (file-string "1.txt"))
	(floor 0)
	(idx 0))
    (while (< idx (length input))
      (if (equal (aref input idx) 40) (setq floor (+ floor 1)) (setq floor (- floor 1)))
      (setq idx (+ idx 1))
      (if (< floor 0) (progn
			(message (number-to-string idx))
			(setq idx (length input)))))
    (message (concat "Finished " (number-to-string (length input))))))


(partOne)
(partTwo)
